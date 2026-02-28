#!/usr/bin/env python3
"""
Cross-implementation conformance test harness for KVL.

Runs all implementations against the shared fixtures and compares results.
Supports multiple test categories: core, valid, escape, header, edge, invalid, merge.
"""

import argparse
import json
import os
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple

# Project paths
SCRIPT_DIR = Path(__file__).parent.resolve()
PROJECT_ROOT = SCRIPT_DIR.parent
FIXTURES_ROOT = PROJECT_ROOT / "fixtures"
PYTHON_DIR = PROJECT_ROOT / "python"
GO_DIR = PROJECT_ROOT / "go"
ZIG_DIR = PROJECT_ROOT / "zig"
JS_DIR = PROJECT_ROOT / "javascript"


class Colors:
    """ANSI color codes for terminal output."""
    GREEN = "\033[92m"
    RED = "\033[91m"
    YELLOW = "\033[93m"
    BLUE = "\033[94m"
    RESET = "\033[0m"
    BOLD = "\033[1m"

    @classmethod
    def disable(cls):
        cls.GREEN = ""
        cls.RED = ""
        cls.YELLOW = ""
        cls.BLUE = ""
        cls.RESET = ""
        cls.BOLD = ""


# ---------------------------------------------------------------------------
# Implementation registry
# ---------------------------------------------------------------------------

IMPLEMENTATIONS = {
    "python": {
        "name": "Python (lib)",
        "features": {"parse", "categorical", "merge", "serialize", "header"},
        "description": "Direct Python library import",
    },
    "python-cli": {
        "name": "Python (CLI)",
        "features": {"parse", "categorical", "merge", "serialize", "header"},
        "description": "Python CLI via subprocess",
    },
    "go": {
        "name": "Go",
        "features": {"parse", "categorical", "merge", "serialize"},
        "description": "Go implementation",
    },
    "zig": {
        "name": "Zig",
        "features": {"parse"},
        "description": "Zig implementation (minimal)",
    },
    "javascript": {
        "name": "JavaScript",
        "features": {"parse", "categorical", "merge", "serialize", "header"},
        "description": "JavaScript implementation",
    },
}

# Test categories and their required features
CATEGORIES = {
    "core":    {"dir": "core",    "type": "parse",        "features": {"parse"}},
    "valid":   {"dir": "valid",   "type": "parse",        "features": {"parse"}},
    "escape":  {"dir": "escape",  "type": "parse",        "features": {"parse"}},
    "header":  {"dir": "header",  "type": "parse",        "features": {"parse", "header"}},
    "edge":    {"dir": "edge",    "type": "parse",        "features": {"parse"}},
    "invalid": {"dir": "invalid", "type": "expect-fail",  "features": {"parse"}},
    "merge":   {"dir": "merge",   "type": "merge",        "features": {"parse", "merge"}},
}


# ---------------------------------------------------------------------------
# JSON comparison
# ---------------------------------------------------------------------------

def normalize_json(obj: Any) -> Any:
    """Normalize JSON for comparison: sort object keys recursively."""
    if isinstance(obj, dict):
        return {k: normalize_json(v) for k in sorted(obj.keys()) for v in [obj[k]]}
    elif isinstance(obj, list):
        return [normalize_json(item) for item in obj]
    else:
        return obj


def json_equal(a: Any, b: Any) -> bool:
    return normalize_json(a) == normalize_json(b)


def format_diff(actual: Any, expected: Any, indent: int = 0) -> str:
    lines = []
    prefix = "  " * indent

    if type(actual) != type(expected):
        lines.append(f"{prefix}Type mismatch: {type(actual).__name__} vs {type(expected).__name__}")
        lines.append(f"{prefix}  Actual:   {json.dumps(actual, sort_keys=True)}")
        lines.append(f"{prefix}  Expected: {json.dumps(expected, sort_keys=True)}")
        return "\n".join(lines)

    if isinstance(actual, dict) and isinstance(expected, dict):
        all_keys = set(actual.keys()) | set(expected.keys())
        for key in sorted(all_keys):
            if key not in actual:
                lines.append(f"{prefix}Missing key: {key!r}")
                lines.append(f"{prefix}  Expected: {json.dumps(expected[key], sort_keys=True)}")
            elif key not in expected:
                lines.append(f"{prefix}Extra key: {key!r}")
                lines.append(f"{prefix}  Actual: {json.dumps(actual[key], sort_keys=True)}")
            elif not json_equal(actual[key], expected[key]):
                lines.append(f"{prefix}Key {key!r}:")
                lines.append(format_diff(actual[key], expected[key], indent + 1))
    elif isinstance(actual, list) and isinstance(expected, list):
        if len(actual) != len(expected):
            lines.append(f"{prefix}Length mismatch: {len(actual)} vs {len(expected)}")
        for i, (a, e) in enumerate(zip(actual, expected)):
            if not json_equal(a, e):
                lines.append(f"{prefix}Index {i}:")
                lines.append(format_diff(a, e, indent + 1))
        extra = actual[len(expected):] if len(actual) > len(expected) else expected[len(actual):]
        tag = "Extra" if len(actual) > len(expected) else "Missing"
        start = min(len(actual), len(expected))
        for i, v in enumerate(extra):
            lines.append(f"{prefix}{tag} index {start + i}: {json.dumps(v, sort_keys=True)}")
    else:
        lines.append(f"{prefix}Value mismatch:")
        lines.append(f"{prefix}  Actual:   {json.dumps(actual, sort_keys=True)}")
        lines.append(f"{prefix}  Expected: {json.dumps(expected, sort_keys=True)}")

    return "\n".join(lines)


# ---------------------------------------------------------------------------
# Implementation runners
# ---------------------------------------------------------------------------

def _run_cli(cmd: List[str], stdin_data: str = None, timeout: int = 30, cwd: str = None) -> Tuple[Optional[Any], Optional[str]]:
    """Run a CLI command, return (parsed_json_or_None, error_or_None)."""
    try:
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            input=stdin_data,
            timeout=timeout,
            cwd=cwd,
        )
        if result.returncode != 0:
            return None, result.stderr.strip() or "CLI returned non-zero"
        return json.loads(result.stdout), None
    except subprocess.TimeoutExpired:
        return None, "Timeout"
    except json.JSONDecodeError as e:
        return None, f"Invalid JSON output: {e}"
    except FileNotFoundError:
        return None, "Binary not found"
    except Exception as e:
        return None, str(e)


def _run_cli_raw(cmd: List[str], stdin_data: str = None, timeout: int = 30, cwd: str = None) -> Tuple[Optional[str], Optional[str]]:
    """Run a CLI command, return (raw_stdout, error_or_None)."""
    try:
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            input=stdin_data,
            timeout=timeout,
            cwd=cwd,
        )
        if result.returncode != 0:
            return None, result.stderr.strip() or "CLI returned non-zero"
        return result.stdout, None
    except subprocess.TimeoutExpired:
        return None, "Timeout"
    except FileNotFoundError:
        return None, "Binary not found"
    except Exception as e:
        return None, str(e)


def _run_cli_expect_fail(cmd: List[str], timeout: int = 30, cwd: str = None) -> Tuple[bool, str]:
    """Run a command expecting non-zero exit. Returns (failed_as_expected, message)."""
    try:
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            timeout=timeout,
            cwd=cwd,
        )
        if result.returncode != 0:
            return True, "Correctly rejected"
        return False, "Expected failure but got exit 0"
    except subprocess.TimeoutExpired:
        return False, "Timeout"
    except FileNotFoundError:
        return False, "Binary not found"
    except Exception as e:
        return False, str(e)


# --- Availability checks ---

def check_go_available() -> Tuple[bool, str]:
    if not GO_DIR.exists():
        return False, "Go directory not found"
    try:
        subprocess.run(["go", "version"], capture_output=True, text=True, timeout=10, check=True)
    except (FileNotFoundError, subprocess.CalledProcessError, Exception):
        return False, "Go not installed"

    go_binary = GO_DIR / "kvl"
    if not go_binary.exists():
        try:
            result = subprocess.run(
                ["go", "build", "./cmd/kvl"],
                capture_output=True, text=True, cwd=str(GO_DIR), timeout=60,
            )
            if result.returncode != 0:
                return False, f"Go build failed: {result.stderr}"
        except Exception as e:
            return False, str(e)
    return True, ""


def check_zig_available() -> Tuple[bool, str]:
    if not ZIG_DIR.exists():
        return False, "Zig directory not found"
    zig_binary = ZIG_DIR / "zig-out" / "bin" / "kvl-demo"
    if not zig_binary.exists():
        try:
            result = subprocess.run(
                ["zig", "build"],
                capture_output=True, text=True, cwd=str(ZIG_DIR), timeout=60,
            )
            if result.returncode != 0:
                return False, f"Zig build failed: {result.stderr}"
        except FileNotFoundError:
            return False, "Zig not installed"
        except Exception as e:
            return False, str(e)
    return True, ""


def check_js_available() -> Tuple[bool, str]:
    if not JS_DIR.exists():
        return False, "JavaScript directory not found"
    cli_path = JS_DIR / "src" / "cli.js"
    if not cli_path.exists():
        return False, "JavaScript CLI not found"
    try:
        subprocess.run(["node", "--version"], capture_output=True, text=True, timeout=10, check=True)
    except (FileNotFoundError, subprocess.CalledProcessError, Exception):
        return False, "Node.js not installed"
    return True, ""


# Pre-check cache
_availability_cache: Dict[str, Tuple[bool, str]] = {}


def is_available(impl_key: str) -> Tuple[bool, str]:
    if impl_key not in _availability_cache:
        if impl_key in ("python", "python-cli"):
            _availability_cache[impl_key] = (True, "")
        elif impl_key == "go":
            _availability_cache[impl_key] = check_go_available()
        elif impl_key == "zig":
            _availability_cache[impl_key] = check_zig_available()
        elif impl_key == "javascript":
            _availability_cache[impl_key] = check_js_available()
        else:
            _availability_cache[impl_key] = (False, "Unknown implementation")
    return _availability_cache[impl_key]


# --- Command builders ---

def _python_lib_parse(kvl_path: Path) -> Tuple[Optional[Any], Optional[str]]:
    try:
        sys.path.insert(0, str(PYTHON_DIR))
        import kvl
        content = kvl_path.read_text()
        return kvl.loads(content), None
    except Exception as e:
        return None, str(e)
    finally:
        if str(PYTHON_DIR) in sys.path:
            sys.path.remove(str(PYTHON_DIR))


def _python_lib_parse_raw(kvl_path: Path) -> Tuple[Optional[Any], Optional[str]]:
    try:
        sys.path.insert(0, str(PYTHON_DIR))
        import kvl
        content = kvl_path.read_text()
        return kvl.parse(content), None
    except Exception as e:
        return None, str(e)
    finally:
        if str(PYTHON_DIR) in sys.path:
            sys.path.remove(str(PYTHON_DIR))


def _python_lib_merge(base_path: Path, overlay_path: Path) -> Tuple[Optional[Any], Optional[str]]:
    try:
        sys.path.insert(0, str(PYTHON_DIR))
        import kvl
        base = kvl.parse(base_path.read_text())
        overlay = kvl.parse(overlay_path.read_text())
        merged = kvl.merge(base, overlay)
        return kvl.compact(merged), None
    except Exception as e:
        return None, str(e)
    finally:
        if str(PYTHON_DIR) in sys.path:
            sys.path.remove(str(PYTHON_DIR))


def _python_lib_serialize(data: Any) -> Tuple[Optional[str], Optional[str]]:
    try:
        sys.path.insert(0, str(PYTHON_DIR))
        import kvl
        return kvl.dumps(data), None
    except Exception as e:
        return None, str(e)
    finally:
        if str(PYTHON_DIR) in sys.path:
            sys.path.remove(str(PYTHON_DIR))


def _python_lib_expect_fail(kvl_path: Path) -> Tuple[bool, str]:
    try:
        sys.path.insert(0, str(PYTHON_DIR))
        import kvl
        kvl.loads(kvl_path.read_text())
        return False, "Expected failure but parsed OK"
    except Exception:
        return True, "Correctly rejected"
    finally:
        if str(PYTHON_DIR) in sys.path:
            sys.path.remove(str(PYTHON_DIR))


def get_parse_cmd(impl_key: str, kvl_path: Path) -> Tuple[Optional[Any], Optional[str]]:
    if impl_key == "python":
        return _python_lib_parse(kvl_path)
    elif impl_key == "python-cli":
        return _run_cli([sys.executable, "-m", "kvl", "convert", str(kvl_path)], timeout=30, cwd=str(PYTHON_DIR))
    elif impl_key == "go":
        return _run_cli([str(GO_DIR / "kvl"), "parse", str(kvl_path)])
    elif impl_key == "zig":
        return _run_cli([str(ZIG_DIR / "zig-out" / "bin" / "kvl-demo"), "parse-json", str(kvl_path)])
    elif impl_key == "javascript":
        return _run_cli(["node", str(JS_DIR / "src" / "cli.js"), "parse", str(kvl_path)])
    return None, "Unknown implementation"


def get_parse_raw_cmd(impl_key: str, kvl_path: Path) -> Tuple[Optional[Any], Optional[str]]:
    if impl_key == "python":
        return _python_lib_parse_raw(kvl_path)
    elif impl_key == "python-cli":
        return _run_cli([sys.executable, "-m", "kvl", "parse-raw", str(kvl_path)], timeout=30, cwd=str(PYTHON_DIR))
    elif impl_key == "go":
        return _run_cli([str(GO_DIR / "kvl"), "parse-raw", str(kvl_path)])
    elif impl_key == "javascript":
        return _run_cli(["node", str(JS_DIR / "src" / "cli.js"), "parse-raw", str(kvl_path)])
    return None, "Not supported"


def get_merge_cmd(impl_key: str, base_path: Path, overlay_path: Path) -> Tuple[Optional[Any], Optional[str]]:
    if impl_key == "python":
        return _python_lib_merge(base_path, overlay_path)
    elif impl_key == "python-cli":
        return _run_cli([sys.executable, "-m", "kvl", "merge", str(base_path), str(overlay_path)], timeout=30, cwd=str(PYTHON_DIR))
    elif impl_key == "go":
        return _run_cli([str(GO_DIR / "kvl"), "merge", str(base_path), str(overlay_path)])
    elif impl_key == "javascript":
        return _run_cli(["node", str(JS_DIR / "src" / "cli.js"), "merge", str(base_path), str(overlay_path)])
    return None, "Not supported"


def get_serialize_cmd(impl_key: str, data: Any) -> Tuple[Optional[str], Optional[str]]:
    json_str = json.dumps(data, ensure_ascii=False)
    if impl_key == "python":
        return _python_lib_serialize(data)
    elif impl_key == "python-cli":
        return _run_cli_raw([sys.executable, "-m", "kvl", "serialize"], stdin_data=json_str, timeout=30, cwd=str(PYTHON_DIR))
    elif impl_key == "go":
        return _run_cli_raw([str(GO_DIR / "kvl"), "serialize"], stdin_data=json_str)
    elif impl_key == "javascript":
        return _run_cli_raw(["node", str(JS_DIR / "src" / "cli.js"), "serialize"], stdin_data=json_str)
    return None, "Not supported"


def get_expect_fail_cmd(impl_key: str, kvl_path: Path) -> Tuple[bool, str]:
    if impl_key == "python":
        return _python_lib_expect_fail(kvl_path)
    elif impl_key == "python-cli":
        return _run_cli_expect_fail([sys.executable, "-m", "kvl", "convert", str(kvl_path)], cwd=str(PYTHON_DIR))
    elif impl_key == "go":
        return _run_cli_expect_fail([str(GO_DIR / "kvl"), "parse", str(kvl_path)])
    elif impl_key == "zig":
        return _run_cli_expect_fail([str(ZIG_DIR / "zig-out" / "bin" / "kvl-demo"), "parse-json", str(kvl_path)])
    elif impl_key == "javascript":
        return _run_cli_expect_fail(["node", str(JS_DIR / "src" / "cli.js"), "parse", str(kvl_path)])
    return False, "Unknown implementation"


# ---------------------------------------------------------------------------
# Test execution
# ---------------------------------------------------------------------------

class TestResult:
    def __init__(self, status: str, label: str, message: str = "",
                 actual: Any = None, expected: Any = None):
        self.status = status    # "pass", "fail", "skip"
        self.label = label      # e.g. "core/simple [parse]"
        self.message = message
        self.actual = actual
        self.expected = expected


def run_parse_tests(category: str, fixtures_dir: Path, impl_keys: List[str],
                    verbose: bool) -> List[Tuple[str, str, TestResult]]:
    """Run parse tests for a category. Returns list of (fixture_label, impl_key, result)."""
    results = []

    for kvl_file in sorted(fixtures_dir.glob("*.kvl")):
        fixture_name = kvl_file.stem
        expected_file = kvl_file.with_suffix(".expected.json")
        cat_expected_file = kvl_file.with_name(f"{fixture_name}.categorical.expected.json")

        for impl_key in impl_keys:
            impl = IMPLEMENTATIONS[impl_key]
            avail, reason = is_available(impl_key)
            if not avail:
                results.append((f"{category}/{fixture_name}", impl_key,
                                TestResult("skip", f"{category}/{fixture_name} [parse]", reason)))
                continue

            # Compacted parse test
            if expected_file.exists():
                cat_features = CATEGORIES[category]["features"]
                if not cat_features.issubset(impl["features"]):
                    results.append((f"{category}/{fixture_name}", impl_key,
                                    TestResult("skip", f"{category}/{fixture_name} [parse]", "Missing features")))
                else:
                    expected = json.loads(expected_file.read_text())
                    actual, error = get_parse_cmd(impl_key, kvl_file)
                    label = f"{category}/{fixture_name} [parse]"
                    if error:
                        results.append((f"{category}/{fixture_name}", impl_key,
                                        TestResult("fail", label, f"Error: {error}", expected=expected)))
                    elif json_equal(actual, expected):
                        results.append((f"{category}/{fixture_name}", impl_key,
                                        TestResult("pass", label)))
                    else:
                        results.append((f"{category}/{fixture_name}", impl_key,
                                        TestResult("fail", label, "Output mismatch",
                                                   actual=actual, expected=expected)))

            # Categorical parse test
            cat_features = CATEGORIES[category]["features"] | {"categorical"}
            if cat_expected_file.exists() and cat_features.issubset(impl["features"]):
                expected_cat = json.loads(cat_expected_file.read_text())
                actual_cat, error_cat = get_parse_raw_cmd(impl_key, kvl_file)
                label_cat = f"{category}/{fixture_name} [categorical]"
                if error_cat:
                    results.append((f"{category}/{fixture_name}", impl_key,
                                    TestResult("fail", label_cat, f"Error: {error_cat}", expected=expected_cat)))
                elif json_equal(actual_cat, expected_cat):
                    results.append((f"{category}/{fixture_name}", impl_key,
                                    TestResult("pass", label_cat)))
                else:
                    results.append((f"{category}/{fixture_name}", impl_key,
                                    TestResult("fail", label_cat, "Output mismatch",
                                               actual=actual_cat, expected=expected_cat)))

    return results


def run_invalid_tests(fixtures_dir: Path, impl_keys: List[str],
                      verbose: bool) -> List[Tuple[str, str, TestResult]]:
    """Run invalid fixture tests (expect parsing to fail)."""
    results = []

    for kvl_file in sorted(fixtures_dir.glob("*.kvl")):
        fixture_name = kvl_file.stem

        for impl_key in impl_keys:
            avail, reason = is_available(impl_key)
            if not avail:
                results.append((f"invalid/{fixture_name}", impl_key,
                                TestResult("skip", f"invalid/{fixture_name}", reason)))
                continue

            passed, msg = get_expect_fail_cmd(impl_key, kvl_file)
            label = f"invalid/{fixture_name}"
            if passed:
                results.append((f"invalid/{fixture_name}", impl_key,
                                TestResult("pass", label, msg)))
            else:
                results.append((f"invalid/{fixture_name}", impl_key,
                                TestResult("fail", label, msg)))

    return results


def run_merge_tests(fixtures_dir: Path, impl_keys: List[str],
                    verbose: bool) -> List[Tuple[str, str, TestResult]]:
    """Run merge tests from subdirectories."""
    results = []

    for merge_dir in sorted(d for d in fixtures_dir.iterdir() if d.is_dir()):
        merge_name = merge_dir.name
        base_file = merge_dir / "base.kvl"
        overlay_file = merge_dir / "overlay.kvl"
        expected_file = merge_dir / "expected.json"
        cat_expected_file = merge_dir / "categorical.expected.json"

        if not base_file.exists() or not overlay_file.exists():
            continue

        for impl_key in impl_keys:
            impl = IMPLEMENTATIONS[impl_key]
            avail, reason = is_available(impl_key)
            if not avail:
                results.append((f"merge/{merge_name}", impl_key,
                                TestResult("skip", f"merge/{merge_name}", reason)))
                continue

            if not {"parse", "merge"}.issubset(impl["features"]):
                results.append((f"merge/{merge_name}", impl_key,
                                TestResult("skip", f"merge/{merge_name}", "Missing merge feature")))
                continue

            # Compacted merge test
            if expected_file.exists():
                expected = json.loads(expected_file.read_text())
                actual, error = get_merge_cmd(impl_key, base_file, overlay_file)
                label = f"merge/{merge_name} [compacted]"
                if error:
                    results.append((f"merge/{merge_name}", impl_key,
                                    TestResult("fail", label, f"Error: {error}", expected=expected)))
                elif json_equal(actual, expected):
                    results.append((f"merge/{merge_name}", impl_key,
                                    TestResult("pass", label)))
                else:
                    results.append((f"merge/{merge_name}", impl_key,
                                    TestResult("fail", label, "Output mismatch",
                                               actual=actual, expected=expected)))

    return results


def run_round_trip_tests(categories: List[str], impl_keys: List[str],
                         verbose: bool) -> List[Tuple[str, str, TestResult]]:
    """Run round-trip tests: parse → serialize → parse, compare."""
    results = []
    # Only run on categories that aren't header-dependent
    rt_categories = [c for c in categories if c not in ("header", "invalid", "merge")]

    for category in rt_categories:
        cat_info = CATEGORIES[category]
        fixtures_dir = FIXTURES_ROOT / cat_info["dir"]
        if not fixtures_dir.exists():
            continue

        for kvl_file in sorted(fixtures_dir.glob("*.kvl")):
            fixture_name = kvl_file.stem

            for impl_key in impl_keys:
                impl = IMPLEMENTATIONS[impl_key]
                avail, reason = is_available(impl_key)
                if not avail or "serialize" not in impl["features"]:
                    continue

                label = f"{category}/{fixture_name} [round-trip]"

                # Step 1: parse → JSON A
                actual_a, error_a = get_parse_cmd(impl_key, kvl_file)
                if error_a:
                    results.append((f"{category}/{fixture_name}", impl_key,
                                    TestResult("fail", label, f"Parse error: {error_a}")))
                    continue

                # Step 2: serialize JSON A → KVL text B
                kvl_text, error_ser = get_serialize_cmd(impl_key, actual_a)
                if error_ser:
                    results.append((f"{category}/{fixture_name}", impl_key,
                                    TestResult("fail", label, f"Serialize error: {error_ser}")))
                    continue

                # Step 3: write B to temp file, parse again → JSON C
                with tempfile.NamedTemporaryFile(mode='w', suffix='.kvl', delete=False) as tmp:
                    tmp.write(kvl_text)
                    tmp_path = Path(tmp.name)

                try:
                    actual_c, error_c = get_parse_cmd(impl_key, tmp_path)
                    if error_c:
                        results.append((f"{category}/{fixture_name}", impl_key,
                                        TestResult("fail", label, f"Re-parse error: {error_c}")))
                        continue

                    # Step 4: compare A == C
                    if json_equal(actual_a, actual_c):
                        results.append((f"{category}/{fixture_name}", impl_key,
                                        TestResult("pass", label)))
                    else:
                        results.append((f"{category}/{fixture_name}", impl_key,
                                        TestResult("fail", label, "Round-trip mismatch",
                                                   actual=actual_c, expected=actual_a)))
                finally:
                    tmp_path.unlink(missing_ok=True)

    return results


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(
        description="KVL cross-implementation conformance test harness"
    )
    parser.add_argument("--verbose", "-v", action="store_true",
                        help="Show detailed output including diffs")
    parser.add_argument("--impl", choices=list(IMPLEMENTATIONS.keys()),
                        action="append", dest="impls",
                        help="Test only specific implementation(s)")
    parser.add_argument("--no-color", action="store_true",
                        help="Disable colored output")
    parser.add_argument("--list-impls", action="store_true",
                        help="List available implementations and exit")
    parser.add_argument("--core", action="store_true",
                        help="Only run core fixtures")
    parser.add_argument("--category", choices=list(CATEGORIES.keys()),
                        action="append", dest="categories",
                        help="Run specific category(ies)")
    parser.add_argument("--round-trip", action="store_true",
                        help="Include round-trip tests (parse → serialize → parse)")
    parser.add_argument("--fixture",
                        help="Run only a specific fixture (name without extension)")

    args = parser.parse_args()

    if args.no_color or not sys.stdout.isatty():
        Colors.disable()

    if args.list_impls:
        print("Available implementations:")
        for key, impl in IMPLEMENTATIONS.items():
            features = ", ".join(sorted(impl["features"]))
            print(f"  {key:12} - {impl['description']}  [{features}]")
        return 0

    # Determine categories to run
    if args.core:
        run_categories = ["core"]
    elif args.categories:
        run_categories = args.categories
    else:
        run_categories = list(CATEGORIES.keys())

    # Determine implementations to test
    impl_keys = args.impls if args.impls else list(IMPLEMENTATIONS.keys())

    # Print header
    print(f"\n{Colors.BOLD}KVL Conformance Test Harness{Colors.RESET}")
    print(f"Categories: {', '.join(run_categories)}")
    print(f"Implementations: {', '.join(impl_keys)}")
    if args.round_trip:
        print(f"{Colors.BLUE}Round-trip tests enabled{Colors.RESET}")
    print()

    # Collect all results
    all_results: List[Tuple[str, str, TestResult]] = []

    for category in run_categories:
        cat_info = CATEGORIES[category]
        fixtures_dir = FIXTURES_ROOT / cat_info["dir"]

        if not fixtures_dir.exists():
            print(f"{Colors.YELLOW}Warning: fixtures directory {fixtures_dir} not found, skipping {category}{Colors.RESET}")
            continue

        if cat_info["type"] == "parse":
            all_results.extend(run_parse_tests(category, fixtures_dir, impl_keys, args.verbose))
        elif cat_info["type"] == "expect-fail":
            all_results.extend(run_invalid_tests(fixtures_dir, impl_keys, args.verbose))
        elif cat_info["type"] == "merge":
            all_results.extend(run_merge_tests(fixtures_dir, impl_keys, args.verbose))

    # Round-trip tests
    if args.round_trip:
        all_results.extend(run_round_trip_tests(run_categories, impl_keys, args.verbose))

    # Filter by fixture name if requested
    if args.fixture:
        all_results = [(f, i, r) for f, i, r in all_results if args.fixture in f]

    # Print verbose diffs
    if args.verbose:
        for fixture_label, impl_key, result in all_results:
            if result.status == "fail" and result.actual is not None and result.expected is not None:
                print(f"\n{Colors.YELLOW}Diff for {result.label} ({impl_key}):{Colors.RESET}")
                print(format_diff(result.actual, result.expected))

    # Tally
    total_pass = sum(1 for _, _, r in all_results if r.status == "pass")
    total_fail = sum(1 for _, _, r in all_results if r.status == "fail")
    total_skip = sum(1 for _, _, r in all_results if r.status == "skip")

    # Build results table grouped by fixture
    fixture_impl_results: Dict[str, Dict[str, List[TestResult]]] = {}
    for fixture_label, impl_key, result in all_results:
        fixture_impl_results.setdefault(fixture_label, {}).setdefault(impl_key, []).append(result)

    # Print results table
    print(f"\n{Colors.BOLD}Results:{Colors.RESET}")
    print()

    if not fixture_impl_results:
        print(f"{Colors.YELLOW}No tests executed{Colors.RESET}")
        return 0

    fixture_width = max(len(f) for f in fixture_impl_results.keys()) + 2
    impl_width = max(len(IMPLEMENTATIONS[k]["name"]) for k in impl_keys) + 2

    header = f"{'Fixture':<{fixture_width}}"
    for impl_key in impl_keys:
        header += f"{IMPLEMENTATIONS[impl_key]['name']:<{impl_width}}"
    print(header)
    print("-" * len(header))

    for fixture_label in sorted(fixture_impl_results.keys()):
        row = f"{fixture_label:<{fixture_width}}"
        for impl_key in impl_keys:
            test_results = fixture_impl_results[fixture_label].get(impl_key, [])
            if not test_results:
                status = f"{Colors.YELLOW}--{Colors.RESET}"
                status_padded = status + " " * (impl_width - 2)
            elif all(r.status == "pass" for r in test_results):
                n = len(test_results)
                tag = f"PASS" if n == 1 else f"P:{n}"
                status = f"{Colors.GREEN}{tag}{Colors.RESET}"
                status_padded = status + " " * (impl_width - len(tag))
            elif all(r.status == "skip" for r in test_results):
                status = f"{Colors.YELLOW}SKIP{Colors.RESET}"
                status_padded = status + " " * (impl_width - 4)
            else:
                fails = sum(1 for r in test_results if r.status == "fail")
                passes = sum(1 for r in test_results if r.status == "pass")
                if fails > 0:
                    tag = f"FAIL" if passes == 0 else f"F:{fails}"
                    status = f"{Colors.RED}{tag}{Colors.RESET}"
                    status_padded = status + " " * (impl_width - len(tag))
                else:
                    status = f"{Colors.YELLOW}SKIP{Colors.RESET}"
                    status_padded = status + " " * (impl_width - 4)
            row += status_padded
        print(row)

    # Summary
    print()
    print(f"{Colors.BOLD}Summary:{Colors.RESET}")
    print(f"  {Colors.GREEN}Passed: {total_pass}{Colors.RESET}")
    print(f"  {Colors.RED}Failed: {total_fail}{Colors.RESET}")
    print(f"  {Colors.YELLOW}Skipped: {total_skip}{Colors.RESET}")
    print(f"  Total:  {total_pass + total_fail + total_skip}")

    # List failures
    if total_fail > 0:
        print(f"\n{Colors.BOLD}Failures:{Colors.RESET}")
        for fixture_label, impl_key, result in all_results:
            if result.status == "fail":
                impl_name = IMPLEMENTATIONS[impl_key]["name"]
                print(f"  {result.label} ({impl_name}): {result.message}")

    return 0 if total_fail == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
