;;; kvl-flycheck.el --- Flycheck support for KVL -*- lexical-binding: t -*-

;; Copyright (C) 2024 KVL Authors

;; Author: KVL Authors
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1") (flycheck "32") (kvl-mode "1.0.0"))
;; Keywords: languages, tools, convenience
;; URL: https://github.com/kvl-lang/kvl

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Flycheck checker for KVL (Key-Value Language) files.
;;
;; Features:
;; - Syntax error checking
;; - Schema validation (when schema file is specified)
;; - Eval mode validation (checks if expressions parse correctly)
;;
;; Usage:
;;   (require 'kvl-flycheck)
;;   (add-hook 'kvl-mode-hook #'flycheck-mode)
;;
;; Schema validation:
;;   Set `kvl-flycheck-schema-file' buffer-locally or via file-local variable:
;;
;;   ;; -*- kvl-flycheck-schema-file: "config.schema.kvl" -*-
;;   name = test
;;   port = 8080

;;; Code:

(require 'kvl)
(require 'kvl-schema)

(declare-function flycheck-define-generic-checker "flycheck")
(declare-function flycheck-error-new-at "flycheck")
(defvar flycheck-checkers)

;;; Customization

(defgroup kvl-flycheck nil
  "Flycheck support for KVL."
  :prefix "kvl-flycheck-"
  :group 'kvl
  :group 'flycheck)

(defcustom kvl-flycheck-schema-file nil
  "Path to schema file for validation.
Can be absolute or relative to the buffer file.
Set as a file-local or dir-local variable for per-project schemas."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Schema file path"))
  :group 'kvl-flycheck
  :safe #'stringp)

(defcustom kvl-flycheck-check-eval nil
  "If non-nil, check that eval expressions are valid Lisp."
  :type 'boolean
  :group 'kvl-flycheck
  :safe #'booleanp)

;;; Error Parsing

(defun kvl-flycheck--parse-errors (output checker buffer)
  "Parse KVL errors from OUTPUT for CHECKER in BUFFER."
  (let ((errors nil))
    (dolist (err output)
      (when (plist-get err :message)
        (push (flycheck-error-new-at
               (or (plist-get err :line) 1)
               (plist-get err :column)
               (or (plist-get err :level) 'error)
               (plist-get err :message)
               :checker checker
               :buffer buffer
               :filename (buffer-file-name buffer))
              errors)))
    (nreverse errors)))

;;; Syntax Checking

(defun kvl-flycheck--check-syntax (text)
  "Check KVL TEXT for syntax errors. Returns list of error plists."
  (condition-case err
      (progn
        (kvl-parse text)
        nil)  ; No errors
    (kvl-parse-error
     (let* ((msg (cadr err))
            (line (kvl-flycheck--extract-line-from-error msg)))
       (list (list :line line
                   :level 'error
                   :message msg))))
    (error
     (list (list :line 1
                 :level 'error
                 :message (error-message-string err))))))

(defun kvl-flycheck--extract-line-from-error (msg)
  "Try to extract line number from error MSG."
  (if (string-match "line \\([0-9]+\\)" msg)
      (string-to-number (match-string 1 msg))
    1))

;;; Schema Validation

(defun kvl-flycheck--check-schema (text schema-file buffer-file)
  "Validate KVL TEXT against SCHEMA-FILE. BUFFER-FILE for relative paths."
  (let* ((schema-path (if (file-name-absolute-p schema-file)
                          schema-file
                        (expand-file-name schema-file
                                          (file-name-directory buffer-file))))
         (errors nil))
    (unless (file-exists-p schema-path)
      (push (list :line 1
                  :level 'warning
                  :message (format "Schema file not found: %s" schema-path))
            errors))
    (when (file-exists-p schema-path)
      (condition-case err
          (let ((schema (kvl-schema-from-file schema-path))
                (data (kvl-loads text)))
            (kvl-schema-deserialize schema data))
        (kvl-validation-error
         (let* ((msg (cadr err))
                (line (kvl-flycheck--extract-line-from-error msg)))
           (push (list :line line
                       :level 'error
                       :message (format "Schema: %s" msg))
                 errors)))
        (kvl-type-error
         (let* ((msg (cadr err))
                (line (kvl-flycheck--extract-line-from-error msg)))
           (push (list :line line
                       :level 'error
                       :message (format "Type: %s" msg))
                 errors)))
        (kvl-schema-error
         (push (list :line 1
                     :level 'error
                     :message (format "Schema error: %s" (cadr err)))
               errors))
        (error
         (push (list :line 1
                     :level 'warning
                     :message (format "Schema check failed: %s"
                                      (error-message-string err)))
               errors))))
    (nreverse errors)))

;;; Eval Checking

(defun kvl-flycheck--check-eval (text)
  "Check that eval expressions in TEXT are valid Lisp."
  (let ((errors nil)
        (data (condition-case nil (kvl-loads text) (error nil))))
    (when data
      (kvl-flycheck--check-eval-recursive data 1 errors))
    (nreverse errors)))

(defun kvl-flycheck--check-eval-recursive (data line errors)
  "Recursively check DATA for eval errors, tracking LINE number."
  (cond
   ((and (listp data) (cl-every #'consp data))
    (dolist (pair data)
      (kvl-flycheck--check-eval-recursive (cdr pair) line errors)))
   ((stringp data)
    (when (and (string-match-p "^[[:space:]]*(" data)
               (string-match-p ")[[:space:]]*$" data))
      (condition-case err
          (car (read-from-string data))
        (error
         (push (list :line line
                     :level 'warning
                     :message (format "Invalid Lisp expression: %s"
                                      (error-message-string err)))
               errors)))))))

;;; Main Checker

(defun kvl-flycheck--start (checker callback)
  "Start KVL syntax check for CHECKER, calling CALLBACK with results."
  (let* ((buffer (current-buffer))
         (text (buffer-substring-no-properties (point-min) (point-max)))
         (buffer-file (buffer-file-name))
         (all-errors nil))

    ;; Syntax check
    (setq all-errors (append all-errors (kvl-flycheck--check-syntax text)))

    ;; Schema validation (if configured and syntax is OK)
    (when (and (null all-errors)
               kvl-flycheck-schema-file
               buffer-file)
      (setq all-errors (append all-errors
                               (kvl-flycheck--check-schema
                                text kvl-flycheck-schema-file buffer-file))))

    ;; Eval check (if enabled and syntax is OK)
    (when (and (null all-errors)
               kvl-flycheck-check-eval)
      (setq all-errors (append all-errors (kvl-flycheck--check-eval text))))

    ;; Return results
    (funcall callback
             'finished
             (kvl-flycheck--parse-errors all-errors checker buffer))))

;;;###autoload
(defun kvl-flycheck-setup ()
  "Setup Flycheck for KVL mode."
  (interactive)
  (require 'flycheck)
  (flycheck-define-generic-checker 'kvl
    "A KVL syntax and schema checker."
    :start #'kvl-flycheck--start
    :modes '(kvl-mode)
    :predicate (lambda () (derived-mode-p 'kvl-mode)))
  (add-to-list 'flycheck-checkers 'kvl))

;; Auto-setup when flycheck is loaded
(eval-after-load 'flycheck
  '(kvl-flycheck-setup))

(provide 'kvl-flycheck)

;;; kvl-flycheck.el ends here
