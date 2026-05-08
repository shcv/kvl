const fs = require('fs');
const path = require('path');

const root = path.resolve(__dirname, '..');
const pkg = JSON.parse(fs.readFileSync(path.join(root, 'package.json'), 'utf8'));
const treeSitterConfig = JSON.parse(fs.readFileSync(path.join(root, 'tree-sitter.json'), 'utf8'));

function assertExists(relativePath) {
  const fullPath = path.join(root, relativePath);
  if (!fs.existsSync(fullPath)) {
    throw new Error(`missing required file: ${relativePath}`);
  }
}

function assertJson(relativePath) {
  const fullPath = path.join(root, relativePath);
  JSON.parse(fs.readFileSync(fullPath, 'utf8'));
}

assertJson('package.json');
assertJson('tree-sitter.json');

for (const grammar of treeSitterConfig.grammars || []) {
  assertExists(grammar.path);
  assertExists(grammar.highlights);
  assertExists(grammar.indents);
}

for (const relativePath of [
  'LICENSE',
  'src/grammar.json',
  'src/node-types.json',
  'src/parser.c',
  'src/scanner.c',
  'queries/highlights.scm',
  'queries/indents.scm'
]) {
  assertExists(relativePath);
}

if (pkg.main) {
  assertExists(pkg.main);
}

if (!pkg.scripts || typeof pkg.scripts.validate !== 'string') {
  throw new Error('package.json is missing the validate script');
}

console.log('tree-sitter package metadata looks valid');
