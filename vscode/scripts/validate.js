const fs = require('fs');
const path = require('path');

const root = path.resolve(__dirname, '..');
const pkg = JSON.parse(fs.readFileSync(path.join(root, 'package.json'), 'utf8'));

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

if (!pkg.contributes || !Array.isArray(pkg.contributes.languages) || pkg.contributes.languages.length === 0) {
  throw new Error('package.json does not declare a KVL language contribution');
}

const language = pkg.contributes.languages[0];
assertExists(language.configuration);

for (const grammar of pkg.contributes.grammars || []) {
  assertExists(grammar.path);
}

for (const snippet of pkg.contributes.snippets || []) {
  assertExists(snippet.path);
}

assertJson('language-configuration.json');
assertJson('snippets/kvl.json');
assertJson('syntaxes/kvl.tmLanguage.json');

console.log('vscode package metadata looks valid');
