# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

KVL (Key-Value Language) is a minimalist configuration format with three core principles:
1. **Simplicity**: Clean syntax with minimal boilerplate
2. **Composability**: Mathematical merge operations (semigroup properties)  
3. **Mathematical Soundness**: Associative merging where `(A + B) + C = A + (B + C)`

The project is organized as:
- **spec/** - Language specification and documentation
- **python/** - Python implementation (reference)
- **go/** - Go implementation
- **zig/** - Zig implementation
- **javascript/** - JavaScript implementation
- **fixtures/** - Canonical test fixtures shared across implementations

## Development Commands

### Python Implementation (python/) - **Production Ready**
```bash
cd python/
python -m pytest tests/           # Run all tests
python -m pytest tests/test_parser.py -v  # Run specific test file
pip install -e .                  # Install in development mode
python -c "import kvl; print(kvl.loads('name = test'))"  # Quick test
```

### Go Implementation (go/) - **Working**
```bash
cd go/
go test                          # Run all tests
go test -v                       # Run with verbose output
go test -bench=.                 # Run benchmarks
go build ./cmd/kvl              # Build CLI tool
```

### Zig Implementation (zig/) - **Working**
```bash
cd zig/
zig build                              # Build the library and CLI
zig build test                         # Run all tests
zig build run -- parse-json file.kvl   # Parse a KVL file to JSON
```

### JavaScript Implementation (javascript/) - **Working**
```bash
cd javascript/
npm test                         # Run all tests
npm run test:watch               # Run tests in watch mode
node src/cli.js parse file.kvl   # Parse a KVL file to JSON
```

## Architecture Overview

### Multi-Language Structure
- **python/**: Python implementation with JSON-like API (`load`, `loads`, `dump`, `dumps`)
- **go/**: Go implementation with Parse/Load API and compact/expand transforms
- **zig/**: Zig implementation with complete parser, serializer, and merge operations
- **javascript/**: JavaScript implementation with JSON-like API (`load`, `loads`, `dump`, `dumps`)

### Key Architectural Concepts

**Configuration Merging**: KVL configurations are semigroups with associative merge operations:
- Simple values: later overrides earlier
- Objects: recursive merge  
- Repeated keys: merged into nested categorical structure (NOT arrays)
- Mathematical property: `(A + B) + C = A + (B + C)`

**Important**: Repeated keys like `ports = 8080` and `ports = 8081` create nested structure `ports = { 8080 = {}, 8081 = {} }`, not arrays `[8080, 8081]`.

**Type Inference**: No explicit types - values inferred as strings, numbers, booleans, or nested objects.


### Python Package Structure (python/)
- `parser.py` - KVL parsing to Python dictionaries
- `serializer.py` - Python objects to KVL text serialization
- `errors.py` - Custom exception classes with position info
- `utils.py` - Shared utility functions

## KVL Syntax Reference

```kvl
/= Comments start with /=

/= Simple key-value (strings default, no quotes needed)
name = John Doe
age = 30
active = true

/= Nested objects via indentation
server = 
    host = localhost
    port = 8080

/= Repeated keys create nested structure (NOT arrays)
tags = red
tags = green
tags = blue
/= Results in: tags = { red = {}, green = {}, blue = {} }
```

## Current Issues

### Zig Feature Gaps
- **No header parsing** - custom separators and list markers unsupported
- **No two-tier API** - only has `parse()`, no `loads()`-style compacted API
- **No KVL serialization** - can only output JSON, not KVL text
- **No input size or recursion depth limits** (Python and JS cap at 10MB/100 levels)

### Go Feature Gaps
- **No input size or recursion depth limits**
- **Compact produces sorted lists** - Go map iteration is unordered, so repeated key order is lost in `Compact()`

### Specification Updates Needed
- Document categorical merge as correct behavior (not override)
- Add two-tier API architecture (parse vs loads)
- Clarify escape sequences (separator-only, other backslashes are literals)
- Add tab/space indentation rules (no mixing)
- Clarify boolean parsing is application-layer (not core spec)

### Test Fixture Gaps
- No escape sequence fixtures
- No custom separator or header parsing fixtures
- No list marker fixtures
- Only 1 invalid fixture, 1 merge test pair

## Conformance Testing

Run cross-implementation conformance tests:
```bash
cd conformance/
python run.py --core    # Core fixtures - should pass ALL implementations
python run.py           # Full fixtures - shows implementation differences
python run.py --verbose # Show detailed diffs on failures
```

Fixtures are in `fixtures/`:
- `core/` - Minimal fixtures all implementations must pass
- `valid/` - Extended fixtures (may show implementation differences)
- `invalid/` - Files that should fail parsing
- `merge/` - Merge operation test cases

All 5 implementations (Python lib, Python CLI, Go, Zig, JavaScript) pass all conformance fixtures.

## Examples and Testing

Each implementation has its own examples in `<impl>/examples/`.

- Python: 111 passing tests
- Go: 24 passing tests (API, compatibility, integration, benchmarks)
- Zig: 14 passing tests (parser, merge, JSON output, escapes)
- JavaScript: 111 passing tests

## Memories
- Instead of writing temporary test scripts, add tests to the test framework