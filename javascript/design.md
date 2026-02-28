# CCL: JavaScript Implementation Design

## 1. Overview

This document outlines the architecture and design for a JavaScript implementation of the Categorical Configuration Language (CCL). This implementation will focus on providing a clean API that works well in both Node.js and browser environments, with TypeScript support for enhanced developer experience.

## 2. Architecture

### 2.1 Package Structure

```
ccl/
├── src/
│   ├── index.ts              # Main entry point
│   ├── parser.ts             # Core parsing logic
│   ├── serializer.ts         # Serialization functionality 
│   ├── model.ts              # Optional CclConfig class
│   ├── errors.ts             # Custom error types
│   ├── utils/                # Utility functions
│   │   ├── indentation.ts    # Indentation handling
│   │   ├── types.ts          # TypeScript type definitions
│   │   └── validators.ts     # Value validators
│   └── cli/                  # Command-line interface (Node.js only)
├── dist/                     # Build output
├── tests/                    # Test suite
└── examples/                 # Usage examples
```

### 2.2 Core Components

- **Parser**: Transforms CCL text into JavaScript objects
- **Serializer**: Converts JavaScript objects to CCL format
- **Model**: Optional class-based abstraction (CclConfig)
- **Error Handling**: Custom error classes with detailed context

## 3. Data Representation

### 3.1 Mapping to JavaScript Types

CCL types map naturally to JavaScript's built-in types:

| CCL Type | JavaScript Type |
|----------|----------------|
| String   | `string`       |
| Number   | `number`       |
| Boolean  | `boolean`      |
| Record   | `object`       |
| List     | `Array`        |

### 3.2 TypeScript Type Definitions

```typescript
// Core CCL value types
type CclPrimitive = string | number | boolean;
type CclObject = { [key: string]: CclValue };
type CclArray = CclValue[];
type CclValue = CclPrimitive | CclObject | CclArray;

// Configuration options
interface ParseOptions {
  resolveImports?: boolean;
  importResolver?: (path: string) => Promise<string> | string;
  fileSystem?: FileSystemInterface;
}

interface SerializeOptions {
  indentSize?: number;
  indentChar?: ' ' | '\t';
  lineBreak?: '\n' | '\r\n';
  commentStyle?: '/=' | '#';
}
```

### 3.3 Optional Class-based Model

```typescript
class CclConfig {
  private data: CclObject;
  
  constructor(data?: CclObject) {
    this.data = data || {};
  }
  
  get(path: string, defaultValue?: any): any {
    // Get value at dot-notation path
  }
  
  set(path: string, value: any): this {
    // Set value at dot-notation path
  }
  
  merge(other: CclConfig | CclObject): this {
    // Implement CCL merging rules
    return this;
  }
  
  toObject(): CclObject {
    return { ...this.data };
  }
  
  toString(options?: SerializeOptions): string {
    // Use serializer to convert to CCL
  }
  
  static fromString(text: string, options?: ParseOptions): CclConfig {
    // Use parser to create instance
  }
}
```

## 4. API Design

### 4.1 Core Functions

```typescript
// Parsing functions
function parse(text: string, options?: ParseOptions): CclObject;
async function parseAsync(text: string, options?: ParseOptions): Promise<CclObject>;

function parseFile(filePath: string, options?: ParseOptions): CclObject;
async function parseFileAsync(filePath: string, options?: ParseOptions): Promise<CclObject>;

// Serialization functions
function stringify(data: CclObject, options?: SerializeOptions): string;

function writeFile(filePath: string, data: CclObject, options?: SerializeOptions): void;
async function writeFileAsync(filePath: string, data: CclObject, options?: SerializeOptions): Promise<void>;

// Utility functions
function merge(...configs: CclObject[]): CclObject;
```

### 4.2 Environment-specific APIs

#### Node.js

```typescript
// Node.js specific functionality
function watchFile(filePath: string, callback: (config: CclObject) => void): Watcher;
```

#### Browser

```typescript
// Browser specific functionality
function parseFromFetch(url: string, options?: ParseOptions): Promise<CclObject>;
```

## 5. Parsing Implementation

### 5.1 Parsing Algorithm

1. **Preprocessing**:
   - Split input into lines
   - Remove comments
   - Skip empty lines

2. **First Pass**:
   - Identify key-value pairs
   - Determine indentation levels
   - Build flat list of entries with metadata

3. **Second Pass**:
   - Build nested structure based on indentation
   - Handle repeated keys as arrays
   - Process imports

### 5.2 Parser Design

```typescript
class CclParser {
  private text: string;
  private lines: string[];
  private options: ParseOptions;
  
  constructor(text: string, options?: ParseOptions) {
    this.text = text;
    this.lines = text.split(/\r?\n/);
    this.options = options || {};
  }
  
  parse(): CclObject {
    const entries = this.extractEntries();
    return this.buildStructure(entries);
  }
  
  private extractEntries(): Entry[] {
    // First pass: extract all key-value pairs with indentation info
  }
  
  private buildStructure(entries: Entry[]): CclObject {
    // Second pass: build nested structure
  }
  
  private processImports(result: CclObject): CclObject {
    // Handle imports if enabled
  }
}
```

## 6. Serialization Implementation

### 6.1 Serialization Algorithm

1. **Structure Analysis**:
   - Determine object structure
   - Identify arrays for repeated keys
   - Plan indentation levels

2. **Output Generation**:
   - Convert primitives to strings
   - Apply proper indentation for nesting
   - Handle arrays as repeated keys

### 6.2 Serializer Design

```typescript
class CclSerializer {
  private data: CclObject;
  private options: SerializeOptions;
  
  constructor(data: CclObject, options?: SerializeOptions) {
    this.data = data;
    this.options = {
      indentSize: 4,
      indentChar: ' ',
      lineBreak: '\n',
      commentStyle: '/=',
      ...options
    };
  }
  
  serialize(): string {
    const lines: string[] = [];
    this.serializeObject(this.data, '', lines);
    return lines.join(this.options.lineBreak);
  }
  
  private serializeObject(obj: CclObject, indent: string, lines: string[]): void {
    // Convert object to CCL lines with proper indentation
  }
  
  private serializeArray(key: string, array: any[], indent: string, lines: string[]): void {
    // Convert array to repeated keys
  }
  
  private serializeValue(value: any): string {
    // Convert primitive to string representation
  }
}
```

## 7. Error Handling

### 7.1 Error Types

```typescript
class CclError extends Error {
  constructor(message: string) {
    super(message);
    this.name = 'CclError';
  }
}

class CclParseError extends CclError {
  readonly line: number;
  readonly column: number;
  readonly source: string;
  
  constructor(message: string, line: number, column: number, source: string) {
    super(`${message} at line ${line}, column ${column}`);
    this.name = 'CclParseError';
    this.line = line;
    this.column = column;
    this.source = source;
  }
  
  formatError(): string {
    // Return formatted error with source context
  }
}
```

### 7.2 Error Context

Parse errors will include:
- Line and column numbers
- Source line with error
- Visual indicator of error position

## 8. Platform-specific Considerations

### 8.1 Node.js

- File system access for imports and file operations
- Command-line interface
- Stream support for processing large files

### 8.2 Browser

- No direct file system access (use fetch API)
- Import resolver using fetch
- Export as UMD/ESM module for browser compatibility

## 9. Performance Optimizations

- **Lazy Parsing**: Parse only when needed
- **Memoization**: Cache parsed results
- **Chunking**: Process large files in chunks
- **Stream Processing**: For Node.js environments

## 10. Testing Strategy

### 10.1 Unit Tests

- Parser components
- Serializer components
- Error handling
- Model class methods

### 10.2 Integration Tests

- End-to-end parsing and serialization
- Import resolution
- File system operations

### 10.3 Property Tests

- Round-trip testing (parse → serialize → parse)
- Merge associativity (A + (B + C) = (A + B) + C)

## 11. CLI Design (Node.js)

```
$ ccl parse input.ccl --format=json
$ ccl stringify input.json --output=output.ccl
$ ccl validate config.ccl
$ ccl merge config1.ccl config2.ccl > merged.ccl
$ ccl watch config.ccl --exec="node server.js"
```

## 12. Browser Usage

```html
<script type="module">
  import { parse, stringify } from 'ccl';
  
  // Parse CCL
  const config = parse(`
    server = 
      host = localhost
      port = 3000
  `);
  
  // Use configuration
  console.log(config.server.port); // 3000
  
  // Modify and convert back to CCL
  config.server.port = 8080;
  const updatedCcl = stringify(config);
</script>
```

## 13. Implementation Plan

1. Core parsing functionality
2. Basic serialization
3. Error handling system
4. Class-based model (CclConfig)
5. Node.js file system integration
6. Browser compatibility
7. TypeScript declarations
8. Performance optimizations
9. CLI tool
10. Documentation and examples
11. Testing
12. Packaging and distribution