# KVL Emacs Support

This directory contains a major mode and related helpers for KVL:

- `kvl-mode.el`
- `kvl-schema.el`
- `kvl-flycheck.el`
- `kvl.el`
- `kvl-test.el`

## Install

Add the `emacs/` directory to your `load-path` and require the files you need:

```elisp
(add-to-list 'load-path "/path/to/kvl/emacs")
(require 'kvl-mode)
(require 'kvl-schema)
(require 'kvl-flycheck)
```

`kvl-mode` can be loaded through `use-package` as well:

```elisp
(use-package kvl-mode
  :load-path "/path/to/kvl/emacs"
  :mode "\\.kvl\\'")
```

## Validate

Run the bundled batch tests with:

```sh
cd emacs
emacs --batch -L . -l kvl.el -l kvl-test.el -f ert-run-tests-batch-and-exit
```

The test file exercises parsing, serialization, schema helpers, and the major
mode entry points.
