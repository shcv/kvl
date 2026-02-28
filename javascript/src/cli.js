#!/usr/bin/env node

/**
 * CLI for KVL conformance testing.
 *
 * Usage:
 *   node src/cli.js parse <file>
 *   node src/cli.js convert <file>
 *   node src/cli.js parse-raw <file>
 *   node src/cli.js serialize          (reads JSON from stdin)
 *   node src/cli.js merge <f1> <f2>
 */

import { readFileSync } from 'node:fs';
import { parse, loads } from './parser.js';
import { dumps } from './serializer.js';
import { merge, compact } from './transform.js';

const args = process.argv.slice(2);

if (args.length < 1) {
  process.stderr.write('Usage: kvl <parse|parse-raw|serialize|merge> [args...]\n');
  process.exit(1);
}

const command = args[0];

try {
  switch (command) {
    case 'parse':
    case 'convert': {
      if (args.length < 2) {
        process.stderr.write('Usage: kvl parse <file>\n');
        process.exit(1);
      }
      const text = readFileSync(args[1], 'utf-8');
      const result = loads(text);
      process.stdout.write(JSON.stringify(result, null, 2) + '\n');
      break;
    }

    case 'parse-raw': {
      if (args.length < 2) {
        process.stderr.write('Usage: kvl parse-raw <file>\n');
        process.exit(1);
      }
      const text = readFileSync(args[1], 'utf-8');
      const result = parse(text);
      process.stdout.write(JSON.stringify(result, null, 2) + '\n');
      break;
    }

    case 'serialize': {
      const chunks = [];
      process.stdin.setEncoding('utf-8');
      for await (const chunk of process.stdin) {
        chunks.push(chunk);
      }
      const input = JSON.parse(chunks.join(''));
      const output = dumps(input);
      process.stdout.write(output);
      break;
    }

    case 'merge': {
      if (args.length < 3) {
        process.stderr.write('Usage: kvl merge <file1> <file2>\n');
        process.exit(1);
      }
      const text1 = readFileSync(args[1], 'utf-8');
      const text2 = readFileSync(args[2], 'utf-8');
      const parsed1 = parse(text1);
      const parsed2 = parse(text2);
      const merged = merge(parsed1, parsed2);
      const compacted = compact(merged);
      process.stdout.write(JSON.stringify(compacted, null, 2) + '\n');
      break;
    }

    default:
      process.stderr.write(`Unknown command: ${command}\n`);
      process.exit(1);
  }
} catch (err) {
  process.stderr.write(`Error: ${err.message}\n`);
  process.exit(1);
}
