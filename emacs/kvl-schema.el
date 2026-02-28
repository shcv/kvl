;;; kvl-schema.el --- Schema validation for KVL -*- lexical-binding: t -*-

;; Copyright (C) 2024 KVL Authors

;; Author: KVL Authors
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1") (kvl "1.0.0"))
;; Keywords: languages, configuration, validation
;; URL: https://github.com/kvl-lang/kvl

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Schema validation system for KVL (Key-Value Language).
;;
;; Features:
;; - Field-oriented validation (specs apply by field name, not structure)
;; - Recursive matching through nested data
;; - Type conversion (string -> int, bool, float, list, etc.)
;; - Constraint validation (min, max, pattern, enum, etc.)
;; - Open/closed schemas (allow or reject unknown fields)
;; - Required/optional field control
;;
;; Usage:
;;   ;; Create schema from Lisp
;;   (setq schema (kvl-schema-create
;;                 '(("port" . int)
;;                   ("host" . string)
;;                   ("enabled" . bool))))
;;
;;   ;; Or load from KVL file
;;   (setq schema (kvl-schema-from-file "config.schema.kvl"))
;;
;;   ;; Validate and convert data
;;   (kvl-schema-deserialize schema (kvl-loads "port = 8080"))
;;   ;; => (("port" . 8080))
;;
;; Schema KVL format:
;;   #= kvl 1.0 schema open optional
;;   port = int
;;   host = string
;;   server =
;;       port =
;;           type = int
;;           min = 1
;;           max = 65535

;;; Code:

(require 'cl-lib)
(require 'kvl)

;;; Custom Types and Errors

(define-error 'kvl-schema-error "KVL schema error" 'kvl-error)
(define-error 'kvl-validation-error "KVL validation error" 'kvl-error)
(define-error 'kvl-type-error "KVL type error" 'kvl-error)

;;; Schema Structure

(cl-defstruct (kvl-schema (:constructor kvl-schema--create)
                          (:copier nil))
  "KVL schema for validation."
  (fields nil :type list)      ; Alist of field specs
  (open t :type boolean)       ; Allow unknown fields
  (required nil :type boolean)) ; Fields must be present

;;; Type System

(defconst kvl-schema-type-map
  '(("string" . string)
    ("str" . string)
    ("integer" . integer)
    ("int" . integer)
    ("float" . float)
    ("number" . float)
    ("boolean" . boolean)
    ("bool" . boolean)
    ("list" . list)
    ("array" . list)
    ("object" . object)
    ("dict" . object))
  "Mapping from type names to internal type symbols.")

(defconst kvl-schema-constraint-keywords
  '("type" "min" "max" "pattern" "enum" "required"
    "min-length" "max-length" "min_length" "max_length"
    "min-items" "max-items" "min_items" "max_items")
  "Keywords that indicate a constraint spec (vs nested schema).")

;;; Schema Creation

(defun kvl-schema-create (fields &optional open required)
  "Create a schema with FIELDS specifications.
OPEN allows unknown fields - defaults to t, pass \\='closed for closed schema.
REQUIRED if non-nil requires all matched fields to be present."
  (kvl-schema--create
   :fields (kvl-schema--normalize-fields fields)
   :open (not (eq open 'closed))
   :required required))

(defun kvl-schema--normalize-fields (fields)
  "Normalize FIELDS into consistent internal format."
  (mapcar
   (lambda (pair)
     (let ((name (car pair))
           (spec (cdr pair)))
       (cons name (kvl-schema--normalize-spec spec))))
   fields))

(defun kvl-schema--normalize-spec (spec)
  "Normalize a single field SPEC."
  (cond
   ;; Symbol type: int, string, bool, etc.
   ((symbolp spec)
    spec)
   ;; String type name: "int", "string", etc.
   ((stringp spec)
    (kvl-schema--parse-type-name spec))
   ;; Constraint spec as (type . constraints-alist): (integer . (("min" . 1)))
   ((and (consp spec)
         (symbolp (car spec))
         (listp (cdr spec))
         (or (null (cdr spec))
             (and (consp (cdr spec)) (consp (cadr spec)))))
    ;; Already in (type . constraints) format
    (if (cdr spec)
        spec  ; Has constraints
      (car spec)))  ; Just type, no constraints
   ;; Constraint spec as alist: (("type" . "int") ("min" . "1"))
   ((and (listp spec) (kvl-schema--constraint-spec-p spec))
    (kvl-schema--parse-constraint-spec spec))
   ;; Nested schema: (("host" . string) ("port" . int))
   ((and (listp spec) (cl-every #'consp spec))
    (kvl-schema--normalize-fields spec))
   (t
    (signal 'kvl-schema-error (list (format "Invalid field spec: %S" spec))))))

(defun kvl-schema--constraint-spec-p (spec)
  "Check if SPEC is a constraint specification."
  (and (listp spec)
       (cl-some (lambda (pair)
                  (member (if (stringp (car pair))
                              (car pair)
                            (symbol-name (car pair)))
                          kvl-schema-constraint-keywords))
                spec)))

(defun kvl-schema--parse-type-name (name)
  "Convert type NAME string to internal symbol."
  (let ((type-pair (assoc (downcase name) kvl-schema-type-map)))
    (if type-pair
        (cdr type-pair)
      (signal 'kvl-schema-error (list (format "Unknown type: %s" name))))))

(defun kvl-schema--parse-constraint-spec (spec)
  "Parse constraint SPEC into (type . constraints) form."
  (let* ((type-entry (or (assoc "type" spec)
                         (assoc 'type spec)))
         (type-name (cdr type-entry))
         (base-type (if (stringp type-name)
                        (kvl-schema--parse-type-name type-name)
                      type-name))
         (constraints (cl-remove-if
                       (lambda (pair)
                         (member (if (stringp (car pair))
                                     (car pair)
                                   (symbol-name (car pair)))
                                 '("type")))
                       spec)))
    (if constraints
        (cons base-type constraints)
      base-type)))

;;; Type Conversion

(defun kvl-schema--convert-value (value target-type path)
  "Convert VALUE to TARGET-TYPE, signaling error at PATH on failure."
  (condition-case err
      (pcase target-type
        ('string (kvl-schema--to-string value))
        ('integer (kvl-schema--to-integer value))
        ('float (kvl-schema--to-float value))
        ('boolean (kvl-schema--to-boolean value))
        ('list (kvl-schema--to-list value))
        ('object (kvl-schema--to-object value))
        (_ (signal 'kvl-type-error
                   (list (format "Unknown type %S at %s" target-type path)))))
    (kvl-type-error (signal (car err) (cdr err)))
    (error (signal 'kvl-type-error
                   (list (format "Cannot convert %S to %s at %s: %s"
                                 value target-type path (error-message-string err)))))))

(defun kvl-schema--to-string (value)
  "Convert VALUE to string."
  (cond
   ((stringp value) value)
   ((numberp value) (number-to-string value))
   ((eq value t) "true")
   ((null value) "false")
   (t (format "%S" value))))

(defun kvl-schema--to-integer (value)
  "Convert VALUE to integer."
  (cond
   ((integerp value) value)
   ((floatp value) (truncate value))
   ((stringp value)
    (let ((num (string-to-number value)))
      (if (and (= num 0) (not (string-match-p "^[[:space:]]*0" value)))
          (signal 'kvl-type-error (list (format "Cannot convert '%s' to integer" value)))
        (truncate num))))
   (t (signal 'kvl-type-error (list (format "Cannot convert %S to integer" value))))))

(defun kvl-schema--to-float (value)
  "Convert VALUE to float."
  (cond
   ((floatp value) value)
   ((integerp value) (float value))
   ((stringp value)
    (let ((num (string-to-number value)))
      (if (and (= num 0.0) (not (string-match-p "^[[:space:]]*0" value)))
          (signal 'kvl-type-error (list (format "Cannot convert '%s' to float" value)))
        num)))
   (t (signal 'kvl-type-error (list (format "Cannot convert %S to float" value))))))

(defun kvl-schema--to-boolean (value)
  "Convert VALUE to boolean."
  (cond
   ((eq value t) t)
   ((null value) nil)
   ((stringp value)
    (let ((lower (downcase value)))
      (cond
       ((member lower '("true" "yes" "on" "1")) t)
       ((member lower '("false" "no" "off" "0")) nil)
       (t (signal 'kvl-type-error (list (format "Cannot convert '%s' to boolean" value)))))))
   ((numberp value) (not (zerop value)))
   (t (signal 'kvl-type-error (list (format "Cannot convert %S to boolean" value))))))

(defun kvl-schema--to-list (value)
  "Convert VALUE to list."
  (cond
   ((listp value)
    (if (and value (cl-every #'consp value) (cl-every (lambda (x) (stringp (car x))) value))
        ;; KVL categorical format: (("a" . nil) ("b" . nil)) -> ("a" "b")
        (mapcar #'car value)
      value))
   ((stringp value) (list value))
   (t (signal 'kvl-type-error (list (format "Cannot convert %S to list" value))))))

(defun kvl-schema--to-object (value)
  "Convert VALUE to object (alist)."
  (if (and (listp value) (or (null value) (consp (car value))))
      value
    (signal 'kvl-type-error (list (format "Cannot convert %S to object" value)))))

;;; Constraint Validation

(defun kvl-schema--validate-constraints (value constraints path)
  "Validate VALUE against CONSTRAINTS at PATH."
  (dolist (constraint constraints)
    (let ((key (if (stringp (car constraint))
                   (car constraint)
                 (symbol-name (car constraint))))
          (cval (cdr constraint)))
      (pcase key
        ("min"
         (when (and (numberp value) (< value (kvl-schema--to-float cval)))
           (signal 'kvl-validation-error
                   (list (format "Value %s < min %s at %s" value cval path)))))
        ("max"
         (when (and (numberp value) (> value (kvl-schema--to-float cval)))
           (signal 'kvl-validation-error
                   (list (format "Value %s > max %s at %s" value cval path)))))
        ((or "min-length" "min_length")
         (when (and (stringp value) (< (length value) (kvl-schema--to-integer cval)))
           (signal 'kvl-validation-error
                   (list (format "String length %d < min-length %s at %s"
                                 (length value) cval path)))))
        ((or "max-length" "max_length")
         (when (and (stringp value) (> (length value) (kvl-schema--to-integer cval)))
           (signal 'kvl-validation-error
                   (list (format "String length %d > max-length %s at %s"
                                 (length value) cval path)))))
        ("pattern"
         (when (and (stringp value) (not (string-match-p cval value)))
           (signal 'kvl-validation-error
                   (list (format "Value '%s' doesn't match pattern '%s' at %s"
                                 value cval path)))))
        ("enum"
         (let ((allowed (if (stringp cval)
                            (split-string cval "," t "[[:space:]]+")
                          cval)))
           (unless (member (kvl-schema--to-string value) allowed)
             (signal 'kvl-validation-error
                     (list (format "Value '%s' not in enum %S at %s"
                                   value allowed path))))))
        ((or "min-items" "min_items")
         (when (and (listp value) (< (length value) (kvl-schema--to-integer cval)))
           (signal 'kvl-validation-error
                   (list (format "List length %d < min-items %s at %s"
                                 (length value) cval path)))))
        ((or "max-items" "max_items")
         (when (and (listp value) (> (length value) (kvl-schema--to-integer cval)))
           (signal 'kvl-validation-error
                   (list (format "List length %d > max-items %s at %s"
                                 (length value) cval path)))))))))

;;; Core Validation

(defun kvl-schema-deserialize (schema data &optional path)
  "Validate DATA against SCHEMA and convert to typed values.
PATH is the current location for error reporting."
  (unless (listp data)
    (signal 'kvl-type-error (list (format "Expected alist at %s, got %S"
                                          (or path "root") data))))
  (kvl-schema--validate-recursive schema data nil (or path "")))

(defun kvl-schema--validate-recursive (schema data current-subschema path)
  "Recursively validate DATA against SCHEMA.
CURRENT-SUBSCHEMA is the active nested schema context.
PATH is for error reporting."
  (let ((result nil)
        (fields (kvl-schema-fields schema)))
    ;; Process each key in data
    (dolist (pair data)
      (let* ((key (car pair))
             (value (cdr pair))
             (field-path (if (string-empty-p path) key (concat path "." key)))
             (spec (or (and current-subschema (cdr (assoc key current-subschema)))
                       (cdr (assoc key fields)))))
        (if spec
            ;; Found matching spec - validate
            (push (cons key (kvl-schema--validate-value schema value spec field-path))
                  result)
          ;; No matching spec
          (if (kvl-schema-open schema)
              ;; Open schema - pass through (recurse if nested)
              (if (and (listp value) (cl-every #'consp value))
                  (push (cons key (kvl-schema--validate-recursive
                                   schema value nil field-path))
                        result)
                (push pair result))
            ;; Closed schema - error
            (signal 'kvl-validation-error
                    (list (format "Unknown field '%s' in closed schema" field-path)))))))

    ;; Check required fields if in subschema context
    (when current-subschema
      (kvl-schema--check-required schema data current-subschema path))

    (nreverse result)))

(defun kvl-schema--validate-value (schema value spec path)
  "Validate VALUE against SPEC at PATH."
  (cond
   ;; Simple type symbol
   ((symbolp spec)
    (kvl-schema--convert-value value spec path))

   ;; Constraint spec: (type . constraints)
   ((and (consp spec) (symbolp (car spec)))
    (let* ((base-type (car spec))
           (constraints (cdr spec))
           (converted (kvl-schema--convert-value value base-type path)))
      (kvl-schema--validate-constraints converted constraints path)
      converted))

   ;; Nested subschema
   ((and (listp spec) (cl-every #'consp spec))
    (unless (and (listp value) (or (null value) (consp (car value))))
      (signal 'kvl-type-error
              (list (format "Expected nested object at %s, got %S" path value))))
    (kvl-schema--validate-recursive schema value spec path))

   (t
    (signal 'kvl-schema-error (list (format "Invalid spec at %s: %S" path spec))))))

(defun kvl-schema--check-required (schema data subschema path)
  "Check that required fields in SUBSCHEMA are present in DATA."
  (dolist (spec-pair subschema)
    (let ((field-name (car spec-pair))
          (field-spec (cdr spec-pair)))
      (when (kvl-schema--field-required-p schema field-spec)
        (unless (assoc field-name data)
          (signal 'kvl-validation-error
                  (list (format "Required field '%s' missing at %s"
                                field-name path))))))))

(defun kvl-schema--field-required-p (schema spec)
  "Check if field with SPEC is required according to SCHEMA."
  (cond
   ;; Constraint spec with explicit required
   ((and (consp spec) (symbolp (car spec)))
    (let ((req (cdr (assoc "required" (cdr spec)))))
      (if req
          (kvl-schema--to-boolean req)
        (kvl-schema-required schema))))
   ;; Default to schema setting
   (t (kvl-schema-required schema))))

(defun kvl-schema-validate (schema data &optional path)
  "Validate DATA against SCHEMA, returning t or signaling error."
  (kvl-schema-deserialize schema data path)
  t)

;;; Schema Loading from KVL

(defun kvl-schema-from-kvl (text)
  "Create schema from KVL TEXT."
  (let* ((parsed (kvl-loads text))
         (header (kvl-schema--parse-header text))
         (open (plist-get header :open))
         (required (plist-get header :required))
         (fields (kvl-schema--parse-schema-fields parsed)))
    (kvl-schema--create
     :fields fields
     :open (if (null open) t open)
     :required required)))

(defun kvl-schema-from-file (file)
  "Load schema from KVL FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (kvl-schema-from-kvl (buffer-string))))

(defun kvl-schema--parse-header (text)
  "Parse schema header from TEXT for open/closed and required settings."
  (let ((open t)
        (required nil))
    (when (string-match "^#[^[:space:]]*[[:blank:]]+kvl[[:blank:]]+[0-9.]+" text)
      (let ((header-line (match-string 0 text)))
        (when (string-match-p "closed" header-line)
          (setq open nil))
        (when (string-match-p "required" header-line)
          (setq required t))))
    (list :open open :required required)))

(defun kvl-schema--parse-schema-fields (parsed)
  "Parse PARSED KVL data into schema field specs."
  (let ((fields nil))
    (dolist (pair parsed)
      (let ((name (car pair))
            (def (cdr pair)))
        ;; Skip comments and headers
        (unless (or (string-prefix-p "/" name)
                    (string-prefix-p "#" name))
          (let ((spec (kvl-schema--parse-field-def def)))
            (when spec
              (push (cons name spec) fields))))))
    (nreverse fields)))

(defun kvl-schema--parse-field-def (def)
  "Parse a field definition DEF from schema."
  (cond
   ;; Simple type: "int", "string", etc.
   ((stringp def)
    (kvl-schema--parse-type-name def))

   ;; Alist - could be constraint or nested schema
   ((and (listp def) (cl-every #'consp def))
    (if (assoc "type" def)
        ;; Constraint spec
        (let* ((type-name (cdr (assoc "type" def)))
               (base-type (kvl-schema--parse-type-name type-name))
               (constraints (cl-remove-if
                             (lambda (p) (equal (car p) "type"))
                             def)))
          (if constraints
              (cons base-type constraints)
            base-type))
      ;; Nested schema
      (kvl-schema--parse-schema-fields def)))

   (t nil)))

;;; Convenience Functions

(defun kvl-schema-loads (schema text)
  "Parse KVL TEXT and validate against SCHEMA."
  (kvl-schema-deserialize schema (kvl-loads text)))

(defun kvl-schema-load (schema file)
  "Load KVL FILE and validate against SCHEMA."
  (kvl-schema-deserialize schema (kvl-load file)))

(provide 'kvl-schema)

;;; kvl-schema.el ends here
