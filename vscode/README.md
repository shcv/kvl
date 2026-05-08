# KVL VS Code Extension

Syntax highlighting, snippets, and language configuration for `*.kvl` files.

## Install locally

For extension development:

```sh
code --extensionDevelopmentPath /path/to/kvl/vscode
```

For a normal install:

1. Package the extension into a `.vsix`.
2. Install the archive with `code --install-extension`.

## Package

```sh
cd vscode
npm run validate
npx @vscode/vsce package
```

The package manifest contributes the `kvl` language, the TextMate grammar in
`syntaxes/kvl.tmLanguage.json`, the language configuration, and the snippet set.
The package also includes its own `LICENSE` file so the VSIX carries the stated
CC0-1.0 license metadata cleanly.

## Use

Open a `.kvl` file and VS Code should apply the language automatically. The
extension also provides the base comment and indentation behavior defined in
`language-configuration.json`.
