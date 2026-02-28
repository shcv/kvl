;;; kvl.el --- KVL parser and serializer for Emacs Lisp -*- lexical-binding: t -*-

;; Copyright (C) 2024 KVL Authors

;; Author: KVL Authors
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: languages, configuration, data
;; URL: https://github.com/kvl-lang/kvl

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Full KVL (Key-Value Language) parser and serializer for Emacs Lisp.
;;
;; KVL is a minimalist configuration format with:
;; - Simple key-value syntax: key = value
;; - Indentation-based nesting
;; - Multiple separator support (=, :, ->, :=)
;; - Mathematical merge operations (semigroup properties)
;;
;; This library provides two APIs:
;;
;; Standard (compacted) API:
;;   `kvl-loads' - Parse KVL string to compacted Lisp structure
;;   `kvl-load'  - Parse KVL file to compacted Lisp structure
;;   `kvl-dumps' - Serialize Lisp structure to KVL string
;;   `kvl-dump'  - Serialize Lisp structure to KVL file
;;
;; Low-level (categorical) API:
;;   `kvl-parse' - Parse KVL string to categorical structure
;;   `kvl-merge' - Merge two categorical structures
;;   `kvl-compact' - Convert categorical structure to compacted form
;;
;; Usage:
;;   (require 'kvl)
;;   (kvl-loads "name = John\nage = 30")
;;   => (("name" . "John") ("age" . 30))

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;; Customization

(defgroup kvl-parser nil
  "KVL parser settings."
  :prefix "kvl-"
  :group 'data)

(defcustom kvl-indent-offset 2
  "Number of spaces per indent level for serialization."
  :type 'integer
  :group 'kvl-parser)

(defcustom kvl-type-inference nil
  "If non-nil, automatically infer types for values.
Numbers become integers/floats, true/false become t/nil.
This is disabled by default; type inference should be schema-driven."
  :type 'boolean
  :group 'kvl-parser)

;;; Error Handling

(define-error 'kvl-error "KVL error")
(define-error 'kvl-parse-error "KVL parse error" 'kvl-error)

(defun kvl--error (format-string &rest args)
  "Signal a KVL parse error with FORMAT-STRING and ARGS."
  (signal 'kvl-parse-error (list (apply #'format format-string args))))

;;; Configuration Structure

(cl-defstruct (kvl-config (:constructor kvl-config-create)
                          (:copier nil))
  "KVL configuration."
  (separator "=" :type string)
  (list-markers nil :type list)
  (space-before t :type boolean)
  (space-after t :type boolean))

;;; Header Parsing

(defun kvl--parse-header (text)
  "Parse KVL header from TEXT and return (config . content-start-position).
Returns nil if no header is present."
  ;; Use [[:blank:]] (space/tab only) instead of [[:space:]] to avoid matching newlines
  (when (string-match "^#\\([^[:blank:]\n]+\\)[[:blank:]]+kvl[[:blank:]]+\\([0-9.]+\\)\\(?:[[:blank:]]+\\([^[:blank:]\n]*\\)\\)?\\(?:[[:blank:]]+\\([^\n]*\\)\\)?$"
                      text)
    (let ((separator (match-string 1 text))
          (markers (match-string 3 text))
          (options (match-string 4 text))
          (content-start (1+ (match-end 0))))
      (cons (kvl-config-create
             :separator separator
             :list-markers (when (and markers (not (string= markers "")))
                             (string-to-list markers)))
            content-start))))

(defun kvl--extract-content (text config-result)
  "Extract content from TEXT, skipping header if CONFIG-RESULT is non-nil."
  (if config-result
      (substring text (cdr config-result))
    text))

;;; Escape Handling

(defun kvl--find-unescaped-separator (line separator)
  "Find position of first unescaped SEPARATOR in LINE.
Returns -1 if not found."
  (let ((pos 0)
        (sep-len (length separator))
        (line-len (length line)))
    (catch 'found
      (while (< pos line-len)
        (let ((sep-pos (string-match-p (regexp-quote separator) line pos)))
          (unless sep-pos
            (throw 'found -1))
          (if (and (> sep-pos 0)
                   (= (aref line (1- sep-pos)) ?\\))
              (setq pos (1+ sep-pos))
            (throw 'found sep-pos))))
      -1)))

(defun kvl--unescape-text (text separator)
  "Unescape SEPARATOR in TEXT by removing preceding backslashes."
  (replace-regexp-in-string
   (concat "\\\\" (regexp-quote separator))
   separator
   text))

;;; Low-level Parsing

(defun kvl--is-list-marker (line pos config)
  "Check if POS in LINE contains a valid list marker according to CONFIG."
  (and (kvl-config-list-markers config)
       (< pos (length line))
       (memq (aref line pos) (kvl-config-list-markers config))
       (< (1+ pos) (length line))
       (memq (aref line (1+ pos)) '(?\s ?\t))))

(defun kvl--parse-line (line config)
  "Parse a single LINE according to CONFIG.
Returns (key . value) or nil for empty/comment lines.
Returns (:list-item . value) for list items."
  (let* ((trimmed (string-trim line))
         (indent-chars (- (length line) (length (string-trim-left line))))
         (first-char-pos indent-chars))

    (cond
     ;; Empty line
     ((string-empty-p trimmed)
      nil)

     ;; List marker
     ((kvl--is-list-marker line first-char-pos config)
      (cons :list-item (string-trim (substring line (+ first-char-pos 2)))))

     ;; Key-value pair
     (t
      (let ((sep-pos (kvl--find-unescaped-separator line (kvl-config-separator config))))
        (if (= sep-pos -1)
            ;; No separator - treat as key with empty value
            (cons (string-trim line) "")
          (cons (string-trim (substring line 0 sep-pos))
                (string-trim (substring line (+ sep-pos (length (kvl-config-separator config))))))))))))

(defun kvl--get-indent (line)
  "Get the indentation level of LINE in characters."
  (- (length line) (length (string-trim-left line))))

(defun kvl--parse-block (lines start-idx parent-indent config)
  "Parse a block of lines starting at START-IDX with PARENT-INDENT.
Returns (result . next-index)."
  (let ((result nil)
        (idx start-idx)
        (current-key nil)
        (line-count (length lines))
        (done nil))

    (while (and (< idx line-count) (not done))
      (let* ((line (aref lines idx))
             (indent (kvl--get-indent line))
             (parsed (kvl--parse-line line config)))

        (cond
         ;; Empty line - skip
         ((null parsed)
          (cl-incf idx))

         ;; Line at or before parent indent - end of block
         ((<= indent parent-indent)
          (setq done t))

         ;; List item
         ((eq (car parsed) :list-item)
          (if current-key
              (let ((value (cdr parsed)))
                (push (cons current-key (kvl--make-categorical-value value)) result)
                (cl-incf idx))
            (kvl--error "List item without preceding key at line %d" (1+ idx))))

         ;; Regular key-value
         (t
          (setq current-key (car parsed))
          (let ((value (cdr parsed)))
            (if (string-empty-p value)
                ;; Empty value - check for nested block
                (let* ((next-idx (1+ idx))
                       (next-line (and (< next-idx line-count) (aref lines next-idx)))
                       (next-indent (and next-line (kvl--get-indent next-line))))
                  (if (and next-indent (> next-indent indent))
                      ;; Has nested content
                      (let ((block-result (kvl--parse-block lines next-idx indent config)))
                        (push (cons current-key (kvl--list-to-alist (car block-result))) result)
                        (setq idx (cdr block-result)))
                    ;; No nested content - empty object
                    (push (cons current-key nil) result)
                    (cl-incf idx)))
              ;; Has value
              (push (cons current-key (kvl--make-categorical-value value)) result)
              (cl-incf idx)))))))

    (cons result idx)))

(defun kvl--make-categorical-value (value)
  "Convert VALUE to categorical representation.
Simple values become (value . nil)."
  (if (string-empty-p value)
      nil
    (list (cons value nil))))

(defun kvl--list-to-alist (items)
  "Convert list of (key . value) ITEMS to merged alist.
Duplicate keys are merged categorically, preserving insertion order."
  (let ((result nil))
    (dolist (item (nreverse items))
      (let* ((key (car item))
             (value (cdr item))
             (existing (assoc key result)))
        (if existing
            (setcdr existing (kvl--merge-values (cdr existing) value))
          ;; Append at end to preserve order
          (setq result (append result (list (cons key value)))))))
    result))

(defun kvl--merge-values (a b)
  "Merge two categorical values A and B."
  (cond
   ((null a) b)
   ((null b) a)
   ((and (listp a) (listp b))
    (kvl--merge-alists a b))
   (t b)))

(defun kvl--merge-alists (a b)
  "Merge two alists A and B categorically.
Preserves insertion order from both A and B."
  (let ((result (copy-alist a)))
    (dolist (pair b)
      (let* ((key (car pair))
             (value (cdr pair))
             (existing (assoc key result)))
        (if existing
            (setcdr existing (kvl--merge-values (cdr existing) value))
          ;; Append at end to preserve order
          (setq result (append result (list pair))))))
    result))

;;; Public Parsing Functions

(defun kvl-parse (text &optional config)
  "Parse KVL TEXT into categorical structure.
CONFIG is an optional `kvl-config' struct.

This is the low-level API that preserves the categorical structure.
For a user-friendly compacted format, use `kvl-loads' instead.

Example:
  (kvl-parse \"tags = web\\ntags = api\")
  => ((\"tags\" (\"web\") (\"api\")))"
  (let* ((header-result (kvl--parse-header text))
         (config (or config
                     (if header-result (car header-result) (kvl-config-create))))
         (content (kvl--extract-content text header-result))
         (lines (vconcat (split-string content "\n")))
         (parse-result (kvl--parse-block lines 0 -1 config)))
    (kvl--unescape-alist (kvl--list-to-alist (car parse-result))
                         (kvl-config-separator config))))

(defun kvl--unescape-alist (alist separator)
  "Recursively unescape all keys and values in ALIST."
  (mapcar (lambda (pair)
            (cons (kvl--unescape-text (car pair) separator)
                  (if (listp (cdr pair))
                      (kvl--unescape-alist (cdr pair) separator)
                    (cdr pair))))
          alist))

;;; Compaction

(defun kvl-compact (data)
  "Convert categorical DATA to compacted form.
- Single-entry categorical values become scalars
- Multi-entry categorical values become lists
- Nested structures are recursively compacted
- Type inference is applied if `kvl-type-inference' is non-nil."
  (cond
   ((null data) nil)
   ((and (listp data) (listp (car data)) (stringp (caar data)))
    ;; Alist - process each key
    (mapcar (lambda (pair)
              (let ((key (car pair))
                    (value (cdr pair)))
                (cons key (kvl--compact-value value))))
            data))
   (t data)))

(defun kvl--compact-value (value)
  "Compact a single VALUE."
  (cond
   ;; nil -> nil
   ((null value) nil)

   ;; Single (key . nil) -> infer type of key
   ((and (listp value)
         (= (length value) 1)
         (null (cdar value)))
    (kvl--infer-type (caar value)))

   ;; Multiple entries with nil values -> list
   ((and (listp value)
         (cl-every (lambda (x) (and (consp x) (null (cdr x)))) value))
    (mapcar (lambda (x) (kvl--infer-type (car x))) value))

   ;; Single nested object
   ((and (listp value)
         (= (length value) 1)
         (consp (cdar value)))
    (kvl-compact (cdr (car value))))

   ;; Nested alist
   ((and (listp value) (cl-every #'consp value))
    (kvl-compact value))

   ;; Already a scalar
   (t value)))

(defun kvl--infer-type (str)
  "Infer type of STR and convert if `kvl-type-inference' is non-nil."
  (if (not kvl-type-inference)
      str
    (cond
     ;; Boolean
     ((member (downcase str) '("true" "yes" "on"))
      t)
     ((member (downcase str) '("false" "no" "off"))
      nil)
     ;; Integer
     ((string-match-p "^-?[0-9]+$" str)
      (string-to-number str))
     ;; Float
     ((string-match-p "^-?[0-9]+\\.[0-9]+$" str)
      (string-to-number str))
     ;; String
     (t str))))

;;; High-level API

(defun kvl-loads (text &optional config)
  "Parse KVL TEXT into compacted Lisp structure.
CONFIG is an optional `kvl-config' struct.

This is the standard high-level API that automatically compacts
categorical structures into lists and infers types.

Example:
  (kvl-loads \"name = John\\nage = 30\")
  => ((\"name\" . \"John\") (\"age\" . 30))

  (kvl-loads \"tags = web\\ntags = api\")
  => ((\"tags\" \"web\" \"api\"))"
  (kvl-compact (kvl-parse text config)))

(defun kvl-load (file &optional config)
  "Parse KVL from FILE into compacted Lisp structure.
CONFIG is an optional `kvl-config' struct."
  (with-temp-buffer
    (insert-file-contents file)
    (kvl-loads (buffer-string) config)))

;;; Serialization

(defun kvl-dumps (data &optional config)
  "Serialize DATA to KVL string.
CONFIG is an optional `kvl-config' struct."
  (let ((config (or config (kvl-config-create))))
    (kvl--serialize-alist data 0 config)))

(defun kvl--is-scalar-list-p (value)
  "Check if VALUE is a list of scalars (not an alist)."
  (and (listp value)
       (not (null value))
       (cl-every (lambda (x) (or (stringp x) (numberp x) (eq x t) (eq x nil)))
                 value)))

(defun kvl--is-alist-p (value)
  "Check if VALUE is an alist (list of cons cells with string keys)."
  (and (listp value)
       (not (null value))
       (cl-every (lambda (x) (and (consp x) (stringp (car x))))
                 value)))

(defun kvl--serialize-alist (data indent config)
  "Serialize alist DATA at INDENT level using CONFIG."
  (let ((sep (kvl-config-separator config))
        (indent-str (make-string (* indent kvl-indent-offset) ?\s))
        (lines nil))
    (dolist (pair data)
      (let ((key (car pair))
            (value (cdr pair)))
        (cond
         ;; List of scalars - serialize each as key = item
         ((kvl--is-scalar-list-p value)
          (dolist (item value)
            (push (format "%s%s %s %s" indent-str key sep (kvl--value-to-string item)) lines)))

         ;; Nested alist - serialize as nested block
         ((kvl--is-alist-p value)
          (push (format "%s%s %s" indent-str key sep) lines)
          (push (kvl--serialize-alist value (1+ indent) config) lines))

         ;; Simple value (or nil)
         (t
          (push (format "%s%s %s %s" indent-str key sep (kvl--value-to-string value)) lines)))))
    (string-join (nreverse lines) "\n")))

(defun kvl--value-to-string (value)
  "Convert VALUE to its string representation."
  (cond
   ((null value) "")
   ((eq value t) "true")
   ((stringp value) value)
   ((numberp value) (number-to-string value))
   (t (format "%s" value))))

(defun kvl-dump (data file &optional config)
  "Serialize DATA to KVL and write to FILE.
CONFIG is an optional `kvl-config' struct."
  (with-temp-file file
    (insert (kvl-dumps data config))))

;;; Merge Operations

(defun kvl-merge (a b)
  "Merge two categorical structures A and B.
The merge operation is associative: (merge (merge A B) C) = (merge A (merge B C))."
  (cond
   ((null a) b)
   ((null b) a)
   ((and (listp a) (listp b) (cl-every #'consp a) (cl-every #'consp b))
    (kvl--merge-alists a b))
   (t b)))

;;; Eval Mode

(defcustom kvl-eval-safe-functions
  '(;; Arithmetic
    + - * / % mod max min abs floor ceiling round truncate
    ;; String operations
    concat substring string-to-number number-to-string
    upcase downcase capitalize string-trim string-trim-left string-trim-right
    format format-message
    string= string< string> string-equal string-lessp
    string-match string-match-p string-prefix-p string-suffix-p
    split-string string-join replace-regexp-in-string
    ;; Type conversion
    string-to-number number-to-string
    symbol-name intern intern-soft
    char-to-string string-to-char
    float int-to-string
    ;; Type predicates
    stringp numberp integerp floatp symbolp listp consp null
    zerop plusp minusp
    ;; List operations
    car cdr caar cadr cdar cddr nth nthcdr last butlast
    cons list append reverse length
    member memq assoc assq rassoc rassq alist-get plist-get
    mapcar mapc seq-map seq-filter seq-reduce
    ;; Environment and paths
    getenv
    file-name-directory file-name-nondirectory file-name-extension
    file-name-base file-name-sans-extension
    expand-file-name file-name-as-directory directory-file-name
    file-relative-name file-name-concat
    ;; Comparison
    = /= < > <= >= eq equal
    not and or
    ;; Misc
    identity ignore)
  "List of functions considered safe for `kvl-eval'.
Only expressions using these functions will be evaluated in safe mode."
  :type '(repeat symbol)
  :group 'kvl-parser)

(defun kvl-eval (data &optional unsafe)
  "Evaluate string leaf nodes in DATA as Emacs Lisp expressions.
Returns a new alist with evaluated values.

By default, only expressions using functions in `kvl-eval-safe-functions'
are evaluated. Set UNSAFE to t to evaluate any expression (use with caution).

Leaf values that fail to parse as Lisp or fail evaluation are left as strings.

Example:
  (kvl-eval \\='((\"sum\" . \"(+ 1 2 3)\")
               (\"env\" . \"(getenv \\\"HOME\\\")\")))
  => ((\"sum\" . 6) (\"env\" . \"/home/user\"))"
  (kvl--eval-recursive data unsafe))

(defun kvl--eval-recursive (data unsafe)
  "Recursively evaluate DATA, with UNSAFE controlling evaluation mode."
  (cond
   ;; Alist - recurse into values
   ((and (listp data) (cl-every #'consp data))
    (mapcar (lambda (pair)
              (cons (car pair)
                    (kvl--eval-recursive (cdr pair) unsafe)))
            data))
   ;; List of scalars - evaluate each
   ((listp data)
    (mapcar (lambda (item) (kvl--eval-value item unsafe)) data))
   ;; Scalar value - try to evaluate
   (t (kvl--eval-value data unsafe))))

(defun kvl--eval-value (value unsafe)
  "Try to evaluate VALUE as Lisp. Return VALUE unchanged on failure.
UNSAFE controls whether arbitrary expressions are allowed."
  (if (not (stringp value))
      value
    (condition-case nil
        (let* ((trimmed (string-trim value))
               (expr (car (read-from-string trimmed))))
          (if (or unsafe (kvl--eval-safe-p expr))
              (eval expr t)
            ;; Not safe - return original string
            value))
      ;; Parse or eval error - return original
      (error value))))

(defun kvl--eval-safe-p (expr)
  "Check if EXPR only uses safe functions."
  (cond
   ;; Atoms are safe
   ((not (consp expr)) t)
   ;; Quote is safe
   ((eq (car expr) 'quote) t)
   ;; Function call - check function and args
   ((symbolp (car expr))
    (and (memq (car expr) kvl-eval-safe-functions)
         (cl-every #'kvl--eval-safe-p (cdr expr))))
   ;; Nested list (e.g., ((lambda ...) args)) - not safe
   (t nil)))

(defun kvl-eval-loads (text &optional unsafe)
  "Parse KVL TEXT and evaluate leaf nodes as Lisp.
UNSAFE allows arbitrary expression evaluation if non-nil."
  (kvl-eval (kvl-loads text) unsafe))

(defun kvl-eval-load (file &optional unsafe)
  "Load KVL FILE and evaluate leaf nodes as Lisp.
UNSAFE allows arbitrary expression evaluation if non-nil."
  (kvl-eval (kvl-load file) unsafe))

(provide 'kvl)

;;; kvl.el ends here
