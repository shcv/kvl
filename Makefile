.PHONY: ci conformance js-syntax js-test json-check lint python-lint python-test test tree-sitter-test go-test zig-test

PYTHON ?= python3
NODE ?= node

ci: lint test

lint: python-lint js-syntax json-check

test: python-test js-test go-test zig-test tree-sitter-test conformance

python-lint:
	$(MAKE) -C python lint

python-test:
	$(MAKE) -C python test

js-syntax:
	cd javascript && \
		for file in src/cli.js src/config.js src/errors.js src/index.js src/parser.js src/serializer.js src/transform.js vitest.config.js; do \
			$(NODE) --check "$$file"; \
		done
	cd tree-sitter-kvl && $(NODE) --check grammar.js

js-test:
	cd javascript && npm test

go-test:
	cd go && GOCACHE=$${GOCACHE:-/tmp/kvl-go-cache} go test ./...

zig-test:
	cd zig && zig build test --global-cache-dir $${ZIG_GLOBAL_CACHE_DIR:-/tmp/kvl-zig-cache}

tree-sitter-test:
	cd tree-sitter-kvl && XDG_CACHE_HOME=$${XDG_CACHE_HOME:-/tmp/kvl-tree-sitter-cache} npm test

conformance:
	$(PYTHON) conformance/run.py

json-check:
	$(PYTHON) -c 'import json, pathlib; paths = ["javascript/package.json", "javascript/package-lock.json", "tree-sitter-kvl/package.json", "tree-sitter-kvl/package-lock.json", "tree-sitter-kvl/tree-sitter.json", "vscode/package.json", "vscode/language-configuration.json", "vscode/snippets/kvl.json", "vscode/syntaxes/kvl.tmLanguage.json"]; [json.loads(pathlib.Path(path).read_text()) for path in paths]'
