# kvl (Python)

Key-Value Language (KVL) is a minimalist configuration format with mathematical merge semantics and a small Python API for parsing, validation, transformation, and serialization.

KVL is inspired by the Categorical Configuration Language (CCL) by Dmitrii Kovanikov.

For the full language specification, see the [KVL specification](https://github.com/shcv/kvl/tree/main/spec).

## Installation

```bash
pip install kvl

# Or install from source
git clone https://github.com/shcv/kvl
cd kvl/python
pip install -e .
```

## Quick Start

```python
import kvl

config = kvl.loads("""
server =
  host = localhost
  port = 8080
  debug = true
database =
  url = postgresql://localhost/myapp
  connections = 10
""")

print(config["server"]["host"])
print(config["server"]["port"])
```

## Documentation

- [Python documentation index](docs/README.org)
- [Examples](examples/)
- [Source specification](https://github.com/shcv/kvl/tree/main/spec)

## License

CC0-1.0
