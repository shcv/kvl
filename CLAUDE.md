# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

KVL (Key-Value Language) is a minimalist configuration format with three core principles:
1. **Simplicity**: Clean syntax with minimal boilerplate
2. **Composability**: Mathematical merge operations (monoid properties)
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

**Configuration Merging**: KVL configurations form a commutative monoid with associative merge operations:
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

## Known Limitations

- Emacs mode has no input size or recursion depth limits

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
- `escape/` - Escape handling fixtures
- `header/` - Custom separator and list marker fixtures
- `edge/` - Edge cases (empty values, unicode, multiline, continuation)
- `invalid/` - Files that should fail parsing (bad indent, depth limit)
- `merge/` - Merge operation test cases (including associativity)

All 5 implementations (Python lib, Python CLI, Go, Zig, JavaScript) pass all conformance fixtures (410/410 with round-trip).

## Examples and Testing

Each implementation has its own examples in `<impl>/examples/`.

- Python: 111 passing tests
- Go: 57 passing tests (API, compatibility, integration, benchmarks)
- Zig: 36 passing tests (parser, merge, JSON output, escapes, serialization)
- JavaScript: 111 passing tests

## Memories
- Instead of writing temporary test scripts, add tests to the test framework