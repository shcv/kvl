# KVL JavaScript Implementation

JavaScript implementation of the Key-Value Language (KVL) parser, serializer, and merge operations.

## Installation

```bash
npm install kvl
```

## Usage

```javascript
import { loads, dumps, parse, merge, compact } from 'kvl';

// Parse KVL text to a compacted JavaScript object
const config = loads(`
server =
    host = localhost
    port = 8080
    debug = true
`);

console.log(config.server.host);    // "localhost"
console.log(config.server.port);    // "8080"

// Serialize a JavaScript object to user-facing KVL text
const kvlText = dumps(config, undefined, { publicFormat: true });

// Nested lists or object-list items need list markers
const marked = dumps(
  {
    groups: [['a', 'b'], ['c']],
    servers: [
      { name: 'web1', port: '80' },
      { name: 'web2', port: '81' },
    ],
    nothing: null,
  },
  new KvlConfig({ listMarkers: '-' }),
  { includeHeader: true }
);

// Low-level categorical parse (preserves repeated key structure)
const raw = parse(`
tags = red
tags = green
tags = blue
`);
// raw.tags = { red: {}, green: {}, blue: {} }

// Merge two configurations
const merged = compact(merge(parse(base), parse(overlay)));
```

## CLI

```bash
# Parse KVL to JSON (compacted)
node src/cli.js parse config.kvl

# Parse KVL to JSON (raw categorical)
node src/cli.js parse-raw config.kvl

# Serialize JSON to KVL (reads from stdin)
echo '{"name": "test"}' | node src/cli.js serialize

# Merge two KVL files
node src/cli.js merge base.kvl overlay.kvl
```

The CLI uses the default serializer surface. If you need `publicFormat`,
explicit `null` rendering, or list-marker control when writing KVL, use the
library API.

## API

- `loads(text)` - Parse KVL text to compacted JavaScript object
- `load(filepath)` - Parse KVL file to compacted JavaScript object
- `parse(text)` - Parse KVL text to raw categorical structure
- `dumps(obj, config, options)` - Serialize JavaScript object to KVL text
- `dump(obj, filepath)` - Serialize JavaScript object to KVL file
- `merge(a, b)` - Merge two categorical structures
- `compact(obj)` - Convert categorical structure to compacted form
- `expand(obj)` - Convert compacted form to categorical structure

If you want scalar values and repeated-key string lists rendered directly in the
output, pass `{ publicFormat: true }`. That mode writes `null` values as the
literal text `null`.

For nested lists or list items that are objects, configure list markers and
include a header:

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

## Testing

```bash
npm test             # Run all tests
npm run test:watch   # Run tests in watch mode
```
