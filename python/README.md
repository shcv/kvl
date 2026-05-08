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

To render compact user-facing data back to plain scalar KVL, use
`public_format=True`:

```python
text = kvl.dumps(config, public_format=True)
```

That mode keeps ordinary scalars readable, writes `None` as the literal text
`null`, and uses repeated keys for flat scalar lists:

```python
text = kvl.dumps(
    {"name": "checkout", "tags": ["web", "api"], "nothing": None},
    public_format=True,
)
```

```kvl
name = checkout
tags = web
tags = api
nothing = null
```

For nested lists or list items that are objects, configure list markers and
include a header:

```python
config = kvl.KvlConfig(list_markers="-")
text = kvl.dumps(
    {
        "groups": [["a", "b"], ["c"]],
        "servers": [
            {"name": "web1", "port": "80"},
            {"name": "web2", "port": "81"},
        ],
    },
    config=config,
    include_header=True,
)
```

```kvl
#= kvl 1.0 -
groups =
  -
    - a
    - b
  -
    - c
servers =
  -
    name = web1
    port = 80
  -
    name = web2
    port = 81
```

## Query Data

The `kvl.query` module provides the KVQ query engine for loaded KVL data:

```python
from kvl.query import execute, query

data = kvl.loads("tags = web\ntags = api\n")

assert execute("tags[0]", data) == "web"
assert query("tags = web\ntags = api\n", "tags[] | length") == 2
```

## Documentation

- [Python documentation index](https://github.com/shcv/kvl/blob/main/python/docs/README.org)
- [Examples](https://github.com/shcv/kvl/tree/main/python/examples)
- [Source specification](https://github.com/shcv/kvl/tree/main/spec)

The CLI currently uses the default serializer surface. If you need
`public_format=True` or explicit list-marker control when writing KVL, use the
library API.

## License

CC0-1.0
