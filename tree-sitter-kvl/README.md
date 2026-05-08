# KVL Tree-sitter Grammar

This package publishes the Tree-sitter grammar sources, generated parser, and
editor queries for KVL.

## Install

```sh
cd tree-sitter-kvl
npm install
```

The package ships the generated parser sources, the grammar source, and the
highlight and indent queries used by Tree-sitter-based editors. It does not
currently ship a generated Node binding entrypoint.

## Validate

```sh
cd tree-sitter-kvl
npm run validate
npm test
```

If you want to regenerate the parser from the grammar source, run:

```sh
npm run generate
```

## Use

Tree-sitter consumers should point at `tree-sitter.json` or the published npm
package metadata. The grammar exposes the `source.kvl` scope and supports
`*.kvl` files out of the box.
