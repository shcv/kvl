;;; kvl-mode.el --- Major mode for KVL (Key-Value Language) files -*- lexical-binding: t -*-

;; Copyright (C) 2024 KVL Authors

;; Author: KVL Authors
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: languages, configuration
;; URL: https://github.com/kvl-lang/kvl

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Major mode for editing KVL (Key-Value Language) configuration files.
;;
;; KVL is a minimalist configuration format with:
;; - Simple key-value syntax: key = value
;; - Indentation-based nesting
;; - Multiple separator support (=, :, ->, :=)
;; - Mathematical merge operations (semigroup properties)
;; - List markers (-, +, *) when declared in header
;;
;; Features:
;; - Syntax highlighting for keys, values, comments, headers, separators
;; - Automatic indentation
;; - Electric indent
;; - Imenu support for top-level keys
;; - Comment/uncomment support
;; - Navigation commands
;;
;; Usage:
;;   (require 'kvl-mode)
;;
;; Or with use-package:
;;   (use-package kvl-mode
;;     :mode "\\.kvl\\'")

;;; Code:

(require 'cl-lib)

;;; Customization

(defgroup kvl nil
  "Major mode for editing KVL files."
  :prefix "kvl-"
  :group 'languages)

(defcustom kvl-indent-offset 2
  "Number of spaces for each indentation level in KVL mode."
  :type 'integer
  :group 'kvl)

(defcustom kvl-default-separator "="
  "Default separator for new key-value pairs."
  :type 'string
  :group 'kvl)

;;; Faces

(defgroup kvl-faces nil
  "Faces used in KVL mode."
  :group 'kvl
  :group 'faces)

(defface kvl-key-face
  '((t :inherit font-lock-variable-name-face))
  "Face for KVL keys."
  :group 'kvl-faces)

(defface kvl-separator-face
  '((t :inherit font-lock-builtin-face))
  "Face for KVL separators."
  :group 'kvl-faces)

(defface kvl-value-string-face
  '((t :inherit font-lock-string-face))
  "Face for KVL string values."
  :group 'kvl-faces)

(defface kvl-value-number-face
  '((t :inherit font-lock-constant-face))
  "Face for KVL numeric values."
  :group 'kvl-faces)

(defface kvl-value-boolean-face
  '((t :inherit font-lock-keyword-face))
  "Face for KVL boolean values."
  :group 'kvl-faces)

(defface kvl-comment-face
  '((t :inherit font-lock-comment-face))
  "Face for KVL comments."
  :group 'kvl-faces)

(defface kvl-header-face
  '((t :inherit font-lock-preprocessor-face))
  "Face for KVL headers."
  :group 'kvl-faces)

(defface kvl-list-marker-face
  '((t :inherit font-lock-constant-face))
  "Face for KVL list markers."
  :group 'kvl-faces)

(defface kvl-escape-face
  '((t :inherit font-lock-escape-face))
  "Face for KVL escape sequences."
  :group 'kvl-faces)

;;; Internal Variables

(defvar-local kvl--separator "="
  "Current separator for the buffer, detected from header.")

(defvar-local kvl--list-markers nil
  "List of valid list markers for the buffer, detected from header.")

;;; Syntax Table

(defvar kvl-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Comments: /= ... is a convention, not special syntax
    ;; But we want to treat / as punctuation
    (modify-syntax-entry ?/ "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?\\ "\\" table)
    ;; Strings don't have quotes in KVL
    (modify-syntax-entry ?\" "." table)
    (modify-syntax-entry ?' "." table)
    ;; Underscore is part of words (for keys)
    (modify-syntax-entry ?_ "w" table)
    table)
  "Syntax table for `kvl-mode'.")

;;; Header Detection

(defun kvl--detect-header ()
  "Detect and parse the KVL header if present.
Sets `kvl--separator' and `kvl--list-markers' buffer-local variables."
  (save-excursion
    (goto-char (point-min))
    ;; Use [[:blank:]] (space/tab) instead of [[:space:]] to avoid matching newlines
    (if (looking-at "^#\\([^[:blank:]\n]+\\)[[:blank:]]+kvl[[:blank:]]+[0-9.]+\\(?:[[:blank:]]+\\([^[:blank:]\n]*\\)\\)?")
        (let ((sep (match-string-no-properties 1))
              (markers (match-string-no-properties 2)))
          (setq kvl--separator (if (string= sep "") "=" sep))
          (setq kvl--list-markers (when (and markers (not (string= markers "")))
                                    (string-to-list markers))))
      ;; No header, use defaults
      (setq kvl--separator "=")
      (setq kvl--list-markers nil))))

;;; Font Lock

(defun kvl--build-font-lock-keywords ()
  "Build font-lock keywords based on current buffer settings."
  (let* ((sep (regexp-quote kvl--separator))
         (escaped-sep (concat "\\\\" sep)))
    `(
      ;; Header line
      ("^#[^[:space:]]*[[:space:]]+kvl[[:space:]]+[0-9.]+.*$"
       . 'kvl-header-face)

      ;; Comment convention: /= comment
      ("^[[:space:]]*\\(/=\\)\\(.*\\)$"
       (1 'kvl-comment-face)
       (2 'kvl-comment-face))

      ;; Escape sequences (backslash + separator)
      (,escaped-sep . 'kvl-escape-face)

      ;; List markers (if any defined)
      ,@(when kvl--list-markers
          `((,(concat "^[[:space:]]*\\([" (regexp-quote (apply #'string kvl--list-markers)) "]\\)[[:space:]]")
             1 'kvl-list-marker-face)))

      ;; Key-value with separator - key part
      (,(concat "^[[:space:]]*\\([^" sep "\n]+?\\)[[:space:]]*" sep)
       1 'kvl-key-face)

      ;; Separator itself
      (,(concat "[^\\\\]\\(" sep "\\)")
       1 'kvl-separator-face)

      ;; Separator at line start
      (,(concat "^\\(" sep "\\)")
       1 'kvl-separator-face)

      ;; Boolean values
      (,(concat sep "[[:space:]]*\\(true\\|false\\|yes\\|no\\|on\\|off\\)[[:space:]]*$")
       1 'kvl-value-boolean-face)

      ;; Numeric values (integers and floats)
      (,(concat sep "[[:space:]]*\\(-?[0-9]+\\(?:\\.[0-9]+\\)?\\)[[:space:]]*$")
       1 'kvl-value-number-face))))

(defvar kvl-font-lock-keywords nil
  "Font lock keywords for KVL mode.")

;;; Indentation

(defun kvl-indent-line ()
  "Indent current line as KVL code."
  (interactive)
  (let ((indent (kvl--calculate-indent)))
    (when indent
      (if (<= (current-column) (current-indentation))
          (indent-line-to indent)
        (save-excursion
          (indent-line-to indent))))))

(defun kvl--calculate-indent ()
  "Calculate proper indentation for current line."
  (save-excursion
    (beginning-of-line)
    (cond
     ;; Header lines are not indented
     ((looking-at "^#")
      0)
     ;; First line (no previous content)
     ((bobp)
      0)
     ;; Empty line - keep current or calculate from context
     ((looking-at "^[[:space:]]*$")
      (kvl--previous-line-indent))
     ;; List marker line
     ((and kvl--list-markers
           (looking-at (concat "^[[:space:]]*[" (regexp-quote (apply #'string kvl--list-markers)) "][[:space:]]")))
      (kvl--previous-line-indent))
     ;; Regular line
     (t
      (let ((prev-indent (kvl--previous-line-indent))
            (prev-has-empty-value (kvl--previous-line-has-empty-value-p)))
        (if prev-has-empty-value
            (+ prev-indent kvl-indent-offset)
          prev-indent))))))

(defun kvl--previous-line-indent ()
  "Get the indentation of the previous non-blank line."
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp))
                (looking-at "^[[:space:]]*$"))
      (forward-line -1))
    (current-indentation)))

(defun kvl--previous-line-has-empty-value-p ()
  "Check if the previous non-blank line has an empty value (ends with separator)."
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp))
                (looking-at "^[[:space:]]*$"))
      (forward-line -1))
    (let ((line (buffer-substring-no-properties
                 (line-beginning-position)
                 (line-end-position))))
      (and (string-match (concat (regexp-quote kvl--separator) "[[:space:]]*$") line)
           (not (string-match (concat (regexp-quote kvl--separator) "[[:space:]]*[^[:space:]]") line))))))

;;; Navigation

(defun kvl-beginning-of-defun (&optional arg)
  "Move to the beginning of the current top-level key.
With ARG, move that many top-level keys backward."
  (interactive "p")
  (setq arg (or arg 1))
  (if (> arg 0)
      (while (and (> arg 0)
                  (re-search-backward "^[[:alpha:]_][^[:space:]]*[[:space:]]*=" nil t))
        (setq arg (1- arg)))
    (while (and (< arg 0)
                (re-search-forward "^[[:alpha:]_][^[:space:]]*[[:space:]]*=" nil t))
      (setq arg (1+ arg))
      (beginning-of-line))))

(defun kvl-end-of-defun (&optional arg)
  "Move to the end of the current top-level key's block.
With ARG, move that many blocks forward."
  (interactive "p")
  (setq arg (or arg 1))
  (when (> arg 0)
    (while (> arg 0)
      ;; Find start of next top-level key
      (forward-line 1)
      (if (re-search-forward "^[[:alpha:]_][^[:space:]]*[[:space:]]*=" nil t)
          (progn
            (beginning-of-line)
            (forward-line -1)
            (while (and (not (bobp))
                        (looking-at "^[[:space:]]*$"))
              (forward-line -1))
            (end-of-line))
        (goto-char (point-max)))
      (setq arg (1- arg)))))

(defun kvl-forward-block (&optional arg)
  "Move forward to the next sibling block at the same indent level.
With ARG, move that many blocks."
  (interactive "p")
  (setq arg (or arg 1))
  (let ((target-indent (current-indentation)))
    (while (> arg 0)
      (forward-line 1)
      (while (and (not (eobp))
                  (or (looking-at "^[[:space:]]*$")
                      (> (current-indentation) target-indent)))
        (forward-line 1))
      (setq arg (1- arg)))))

(defun kvl-backward-block (&optional arg)
  "Move backward to the previous sibling block at the same indent level.
With ARG, move that many blocks."
  (interactive "p")
  (setq arg (or arg 1))
  (let ((target-indent (current-indentation)))
    (while (> arg 0)
      (forward-line -1)
      (while (and (not (bobp))
                  (or (looking-at "^[[:space:]]*$")
                      (> (current-indentation) target-indent)))
        (forward-line -1))
      (setq arg (1- arg)))))

;;; Imenu

(defun kvl--imenu-create-index ()
  "Create an Imenu index for KVL buffers.
Returns a nested structure of top-level keys."
  (let ((index nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\([[:alpha:]_][^[:space:]]*\\)[[:space:]]*=" nil t)
        (let ((key (match-string-no-properties 1))
              (pos (match-beginning 0)))
          (push (cons key pos) index))))
    (nreverse index)))

;;; Comments

(defun kvl-comment-dwim (arg)
  "Comment or uncomment current line or region using /= convention.
With ARG, uncomment that many lines."
  (interactive "*P")
  (if (use-region-p)
      (kvl--comment-region (region-beginning) (region-end) arg)
    (kvl--comment-line arg)))

(defun kvl--comment-line (arg)
  "Toggle comment on current line."
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^[[:space:]]*\\(/=\\)")
        ;; Uncomment
        (let ((start (match-beginning 1))
              (end (match-end 1)))
          (delete-region start end)
          (when (looking-at "[[:space:]]")
            (delete-char 1)))
      ;; Comment
      (skip-chars-forward "[:space:]")
      (insert "/= "))))

(defun kvl--comment-region (beg end arg)
  "Comment or uncomment region from BEG to END."
  (save-excursion
    (goto-char beg)
    (beginning-of-line)
    (while (< (point) end)
      (kvl--comment-line arg)
      (forward-line 1))))

;;; Electric Features

(defun kvl-electric-indent-function (_char)
  "Electric indent function for KVL mode.
Returns `do-indent' to trigger indentation."
  (when (eq major-mode 'kvl-mode)
    'do-indent))

;;; Completion

(defun kvl-completion-at-point ()
  "Completion at point function for KVL mode.
Provides completion for boolean values."
  (when (looking-back (concat (regexp-quote kvl--separator) "[[:space:]]*\\([[:alpha:]]*\\)") (line-beginning-position))
    (let ((start (match-beginning 1))
          (end (match-end 1))
          (prefix (match-string 1)))
      (list start end
            '("true" "false" "yes" "no" "on" "off")
            :exclusive 'no))))

;;; Keymap

(defvar kvl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'kvl-comment-dwim)
    (define-key map (kbd "C-c C-n") #'kvl-forward-block)
    (define-key map (kbd "C-c C-p") #'kvl-backward-block)
    (define-key map (kbd "C-M-a") #'kvl-beginning-of-defun)
    (define-key map (kbd "C-M-e") #'kvl-end-of-defun)
    map)
  "Keymap for `kvl-mode'.")

;;; Mode Definition

;;;###autoload
(define-derived-mode kvl-mode prog-mode "KVL"
  "Major mode for editing KVL (Key-Value Language) files.

KVL is a minimalist configuration format with:
- Simple key-value syntax: key = value
- Indentation-based nesting
- Multiple separator support (=, :, ->, :=)
- Mathematical merge operations (semigroup properties)

\\{kvl-mode-map}"
  :syntax-table kvl-mode-syntax-table
  :group 'kvl

  ;; Detect header settings
  (kvl--detect-header)

  ;; Font lock
  (setq-local font-lock-defaults
              (list (kvl--build-font-lock-keywords) nil nil nil nil))

  ;; Indentation
  (setq-local indent-line-function #'kvl-indent-line)
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width kvl-indent-offset)

  ;; Electric indent
  (add-hook 'electric-indent-functions #'kvl-electric-indent-function nil t)

  ;; Comments (using /= convention)
  (setq-local comment-start "/= ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "/=[[:space:]]*")

  ;; Imenu
  (setq-local imenu-create-index-function #'kvl--imenu-create-index)

  ;; Navigation
  (setq-local beginning-of-defun-function #'kvl-beginning-of-defun)
  (setq-local end-of-defun-function #'kvl-end-of-defun)

  ;; Completion
  (add-hook 'completion-at-point-functions #'kvl-completion-at-point nil t)

  ;; Paragraph functions
  (setq-local paragraph-start "^[[:space:]]*$")
  (setq-local paragraph-separate "^[[:space:]]*$"))

;;; File Association

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.kvl\\'" . kvl-mode))

(provide 'kvl-mode)

;;; kvl-mode.el ends here
