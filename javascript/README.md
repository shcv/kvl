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
console.log(config.server.port);    // 8080

// Serialize a JavaScript object to KVL text
const kvlText = dumps(config);

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

## API

- `loads(text)` - Parse KVL text to compacted JavaScript object
- `load(filepath)` - Parse KVL file to compacted JavaScript object
- `parse(text)` - Parse KVL text to raw categorical structure
- `dumps(obj)` - Serialize JavaScript object to KVL text
- `dump(obj, filepath)` - Serialize JavaScript object to KVL file
- `merge(a, b)` - Merge two categorical structures
- `compact(obj)` - Convert categorical structure to compacted form
- `expand(obj)` - Convert compacted form to categorical structure

## Testing

```bash
npm test             # Run all tests
npm run test:watch   # Run tests in watch mode
```
