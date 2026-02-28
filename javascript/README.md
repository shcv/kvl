# ccl-js: Categorical Configuration Language for JavaScript

This is a JavaScript implementation of the Categorical Configuration Language (CCL), a minimalist configuration format focused on simplicity, composability, and mathematical soundness.

**Note: This package is currently in planning stages. See [design.md](design.md) for implementation details.**

## Features (Planned)

- Simple key-value format with minimal syntax
- Hierarchical structure through indentation
- Mathematically sound composition of configurations 
- Support for nested structures, arrays, and common value types
- Clean JavaScript API similar to standard JSON methods
- Browser and Node.js compatibility

## Installation (Coming Soon)

```bash
npm install ccl-js
```

## Quick Start (Preview)

```javascript
import { parse, stringify } from 'ccl-js';

// Parse CCL from a string
const config = parse(`
server = 
    host = localhost
    port = 8080
    debug = true
users = 
    admin = 
        name = Admin User
        roles = admin
        roles = editor
    guest = 
        name = Guest User
        roles = reader
`);

console.log(config.server.host);    // localhost
console.log(config.server.port);    // 8080 (as number)
console.log(config.users.admin.roles);  // ["admin", "editor"]

// Serialize to CCL
const cclText = stringify(config);

// Node.js file operations
import { readFileSync, writeFileSync } from 'fs';

// Load CCL from a file
const configData = readFileSync('config.ccl', 'utf8');
const fileConfig = parse(configData);

// Save to a file
writeFileSync('config.ccl', stringify(config));
```

## CLI (Planned)

```bash
# Convert between formats
ccl-js convert input.json output.ccl

# Validate a CCL file
ccl-js validate config.ccl

# Format a CCL file 
ccl-js format config.ccl

# Merge multiple CCL files
ccl-js merge config1.ccl config2.ccl > merged.ccl
```

## About CCL

CCL is designed to be:

- **Simple**: Minimal syntax, easy to read and write
- **Composable**: Configurations can be merged with well-defined semantics
- **Versatile**: Suitable for configuration files, data exchange, and more