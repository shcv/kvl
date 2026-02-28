;;; kvl-test.el --- Tests for KVL Emacs packages -*- lexical-binding: t -*-

;;; Commentary:

;; Run tests with: emacs --batch -l kvl.el -l kvl-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'kvl)

;;; Parser Tests

(ert-deftest kvl-test-simple-parse ()
  "Test parsing simple key-value pairs."
  (let ((result (kvl-loads "name = John\nage = 30")))
    (should (equal (assoc-default "name" result) "John"))
    (should (equal (assoc-default "age" result) "30"))))

(ert-deftest kvl-test-no-type-inference ()
  "Test that type inference is disabled by default."
  (let ((result (kvl-loads "enabled = true\ncount = 42")))
    (should (equal (assoc-default "enabled" result) "true"))
    (should (equal (assoc-default "count" result) "42"))))

(ert-deftest kvl-test-optional-type-inference ()
  "Test that type inference works when explicitly enabled."
  (let ((kvl-type-inference t))
    (let ((result (kvl-loads "enabled = true\ndisabled = false\ncount = 42")))
      (should (eq (assoc-default "enabled" result) t))
      (should (eq (assoc-default "disabled" result) nil))
      (should (equal (assoc-default "count" result) 42)))))

(ert-deftest kvl-test-nested-structure ()
  "Test parsing nested structures."
  (let* ((input "server =
    host = localhost
    port = 8080")
         (result (kvl-loads input))
         (server (assoc-default "server" result)))
    (should server)
    (should (equal (assoc-default "host" server) "localhost"))
    (should (equal (assoc-default "port" server) "8080"))))

(ert-deftest kvl-test-repeated-keys ()
  "Test that repeated keys create lists."
  (let ((result (kvl-loads "tags = web\ntags = api\ntags = prod")))
    (should (equal (assoc-default "tags" result) '("web" "api" "prod")))))

(ert-deftest kvl-test-comments ()
  "Test that /= comments are parsed as regular keys."
  (let ((result (kvl-loads "/= This is a comment\nname = test")))
    (should (assoc "/" result))
    (should (equal (assoc-default "name" result) "test"))))

(ert-deftest kvl-test-header-separator ()
  "Test custom separator from header."
  (let ((result (kvl-loads "#: kvl 1.0\nhost: localhost\nport: 8080")))
    (should (equal (assoc-default "host" result) "localhost"))
    (should (equal (assoc-default "port" result) "8080"))))

(ert-deftest kvl-test-escape-sequences ()
  "Test escape sequences."
  (let ((result (kvl-loads "equation = x\\=y")))
    (should (equal (assoc-default "equation" result) "x=y"))))

(ert-deftest kvl-test-empty-values ()
  "Test empty values."
  (let ((result (kvl-parse "key =")))
    (should (assoc "key" result))))

;;; Serialization Tests

(ert-deftest kvl-test-serialize-simple ()
  "Test serializing simple structure."
  (let ((data '(("name" . "John") ("age" . 30))))
    (should (string-match-p "name = John" (kvl-dumps data)))
    (should (string-match-p "age = 30" (kvl-dumps data)))))

(ert-deftest kvl-test-serialize-list ()
  "Test serializing lists."
  (let ((data '(("tags" "web" "api"))))
    (should (string-match-p "tags = web" (kvl-dumps data)))
    (should (string-match-p "tags = api" (kvl-dumps data)))))

;;; Merge Tests

(ert-deftest kvl-test-merge-simple ()
  "Test merging two structures."
  (let* ((a (kvl-parse "name = Alice"))
         (b (kvl-parse "age = 30"))
         (merged (kvl-merge a b)))
    (should (assoc "name" merged))
    (should (assoc "age" merged))))

(ert-deftest kvl-test-merge-override ()
  "Test that merge combines repeated keys."
  (let* ((a (kvl-parse "port = 8080"))
         (b (kvl-parse "port = 9000"))
         (merged (kvl-merge a b)))
    ;; Both ports should be present in categorical form
    (should merged)))

;;; Config Tests

(ert-deftest kvl-test-config-create ()
  "Test creating config structures."
  (let ((config (kvl-config-create :separator ":")))
    (should (equal (kvl-config-separator config) ":"))))

;;; Schema Tests

(ert-deftest kvl-test-schema-simple-types ()
  "Test schema with simple type specs."
  (require 'kvl-schema)
  (let ((schema (kvl-schema-create
                 '(("port" . integer)
                   ("host" . string)
                   ("enabled" . boolean)))))
    (let ((result (kvl-schema-deserialize
                   schema
                   '(("port" . "8080")
                     ("host" . "localhost")
                     ("enabled" . "true")))))
      (should (equal (assoc-default "port" result) 8080))
      (should (equal (assoc-default "host" result) "localhost"))
      (should (eq (assoc-default "enabled" result) t)))))

(ert-deftest kvl-test-schema-constraints ()
  "Test schema constraint validation."
  (require 'kvl-schema)
  ;; Create schema with constraint spec: (type . constraints-alist)
  (let ((schema (kvl-schema-create
                 '(("port" . (integer . (("min" . "1") ("max" . "65535"))))))))
    ;; Valid value
    (let ((result (kvl-schema-deserialize schema '(("port" . "8080")))))
      (should (equal (assoc-default "port" result) 8080)))
    ;; Invalid value - should signal error
    (should-error
     (kvl-schema-deserialize schema '(("port" . "70000")))
     :type 'kvl-validation-error)))

(ert-deftest kvl-test-schema-from-kvl ()
  "Test loading schema from KVL text."
  (require 'kvl-schema)
  (let ((schema (kvl-schema-from-kvl "port = int\nhost = string")))
    (let ((result (kvl-schema-deserialize
                   schema
                   '(("port" . "3000") ("host" . "example.com")))))
      (should (equal (assoc-default "port" result) 3000))
      (should (equal (assoc-default "host" result) "example.com")))))

(ert-deftest kvl-test-schema-closed ()
  "Test closed schema rejects unknown fields."
  (require 'kvl-schema)
  (let ((schema (kvl-schema-create '(("port" . integer)) 'closed)))
    (should-error
     (kvl-schema-deserialize schema '(("port" . "80") ("unknown" . "value")))
     :type 'kvl-validation-error)))

(ert-deftest kvl-test-schema-nested ()
  "Test nested schema validation."
  (require 'kvl-schema)
  (let ((schema (kvl-schema-create
                 '(("server" . (("host" . string) ("port" . integer)))))))
    (let ((result (kvl-schema-deserialize
                   schema
                   '(("server" . (("host" . "localhost") ("port" . "8080")))))))
      (should (equal (assoc-default "host" (assoc-default "server" result))
                     "localhost"))
      (should (equal (assoc-default "port" (assoc-default "server" result))
                     8080)))))

;;; Eval Mode Tests

(ert-deftest kvl-test-eval-simple ()
  "Test evaluating simple expressions."
  (let ((data '(("sum" . "(+ 1 2 3)")
                ("name" . "plain string"))))
    (let ((result (kvl-eval data)))
      (should (equal (assoc-default "sum" result) 6))
      (should (equal (assoc-default "name" result) "plain string")))))

(ert-deftest kvl-test-eval-nested ()
  "Test evaluating nested structures."
  (let ((data '(("math" . (("add" . "(+ 10 20)")
                           ("sub" . "(- 100 50)"))))))
    (let ((result (kvl-eval data)))
      (should (equal (assoc-default "add" (assoc-default "math" result)) 30))
      (should (equal (assoc-default "sub" (assoc-default "math" result)) 50)))))

(ert-deftest kvl-test-eval-safe-only ()
  "Test that unsafe functions are blocked by default."
  (let ((data '(("safe" . "(+ 1 2)")
                ("unsafe" . "(shell-command \"echo hi\")"))))
    (let ((result (kvl-eval data)))
      (should (equal (assoc-default "safe" result) 3))
      ;; Unsafe expression should remain as string
      (should (equal (assoc-default "unsafe" result)
                     "(shell-command \"echo hi\")")))))

(ert-deftest kvl-test-eval-getenv ()
  "Test getenv is in safe functions."
  (let ((data '(("home" . "(getenv \"HOME\")"))))
    (let ((result (kvl-eval data)))
      (should (stringp (assoc-default "home" result)))
      (should (not (string-empty-p (assoc-default "home" result)))))))

(ert-deftest kvl-test-eval-loads ()
  "Test kvl-eval-loads convenience function."
  (let ((result (kvl-eval-loads "x = (+ 1 2)\ny = hello")))
    (should (equal (assoc-default "x" result) 3))
    (should (equal (assoc-default "y" result) "hello"))))

(ert-deftest kvl-test-eval-type-conversion ()
  "Test that type conversion functions are safe."
  (let ((result (kvl-eval '(("num" . "(string-to-number \"42\")")
                            ("str" . "(number-to-string 123)")
                            ("sym" . "(symbol-name 'hello)")
                            ("float" . "(float 5)")
                            ("pred" . "(stringp \"test\")")))))
    (should (equal (assoc-default "num" result) 42))
    (should (equal (assoc-default "str" result) "123"))
    (should (equal (assoc-default "sym" result) "hello"))
    (should (equal (assoc-default "float" result) 5.0))
    (should (eq (assoc-default "pred" result) t))))

(provide 'kvl-test)

;;; kvl-test.el ends here
