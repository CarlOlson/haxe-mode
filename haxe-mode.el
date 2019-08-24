;;; haxe-mode.el --- Major mode for Haxe -*- lexical-binding: t -*-

;; Author: Carl Olson <>
;; URL: https://github.com/CarlOlson/haxe-mode
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;; There are other major modes for haxe, this package attempts to
;; provide a simple, modern, and (somewhat) tested alternative.
;; Currently supports Haxe 4.0 keywords.

;; This package is complete and usable.  For support and feature
;; requests please create an issue on github.  PRs are welcome, but
;; consider creating a minor mode for features like autocomplete and
;; linting instead.

;;; Code:

(defcustom haxe-mode-indent-level 4
  "Number of spaces per indent level for ‘haxe-mode’.
`tab-width' is set to this value."
  :type 'integer
  :group 'haxe)

(defcustom haxe-mode-indent-tabs-mode t
  "Sets `indent-tabs-mode' locally if true."
  :type 'boolean
  :group 'haxe)

(defvar haxe-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?' "\"" table)     ;; single quote string
    (modify-syntax-entry ?\" "\"" table)    ;; double quote string
    (modify-syntax-entry ?/ ". 124b" table) ;; /* comment, // comment
    (modify-syntax-entry ?* ". 23" table)   ;; /* comment
    (modify-syntax-entry ?\n "> b" table)   ;; // comment eol
    table)
  "Syntax table for ‘haxe-mode’.")

(defvar haxe-mode-font-lock-keywords
  (let* ((keywords '(abstract cast class dynamic enum extends
                     extern false final final function implements
                     import using in inline interface macro new null
                     operator overload override package public
                     static this true typedef untyped var))
         (controls '(break case catch continue default do else
                     for if return switch throw try while))
         (keyword-strings (mapcar #'symbol-name (append keywords controls)))
         (keywords-regexp (regexp-opt keyword-strings 'words))
         (annotation-regexp   (rx "@:" (group (+ word))))
         (regexp-regexp       (rx "~/" (* (not (any "/"))) "/"))
         (builtin-regexp      (rx (or "trace" "super")))
         (type-regexp         (rx (any "_" upper) (+ (any "_" alphanumeric))))
         (identifier-regexp   (rx (any "_$" alpha) (+ (any "_" alphanumeric))))
         (preprocessor-regexp (rx (or "#if" "#elseif" "#else" "#end"))))
    `((,annotation-regexp 1 font-lock-keyword-face)
      (,regexp-regexp . font-lock-string-face)
      (,builtin-regexp . font-lock-builtin-face)
      (,preprocessor-regexp . font-lock-preprocessor-face)
      (,keywords-regexp . font-lock-keyword-face)
      (,type-regexp . font-lock-type-face)))
  "Font lock configuration for ‘haxe-mode’.")

(defun haxe-indent-line ()
  "Indent current line of code."
  (interactive)
  (save-excursion
    (back-to-indentation)
    (unless (haxe-comment-or-string)
      (haxe-perform-indent)))
  (when (haxe-is-before-indent)
    (haxe-move-to-indent)))

(defun haxe-perform-indent ()
  (indent-line-to (* haxe-mode-indent-level
                     (+ (syntax-ppss-depth (syntax-ppss))
                        (if (looking-at (rx "}")) -1 0)))))

(defun haxe-move-to-indent ()
  (move-to-column (current-indentation)))

(defun haxe-is-before-indent ()
  (> (current-indentation) (current-column)))

(defun haxe-comment-or-string ()
  "Is point in a comment or string?"
  (member (syntax-ppss-context (syntax-ppss)) '(comment string)))

;;;###autoload
(define-derived-mode haxe-mode prog-mode "Haxe"
  "A major mode for editing Haxe files."
  :syntax-table haxe-mode-syntax-table
  (setq-local comment-start "// ")
  (setq-local comment-start-skip (rx "//" (* (or "//" whitespace))))
  (setq-local indent-line-function #'haxe-indent-line)
  (setq-local font-lock-defaults '(haxe-mode-font-lock-keywords))
  (setq-local indent-tabs-mode haxe-mode-indent-tabs-mode)
  (setq-local tab-width haxe-mode-indent-level)
  (setq-local syntax-propertize-function
              (syntax-propertize-rules
               ((rx "~"
                    (group "/")
                    (or "" (and (*? anything) (not (syntax escape))))
                    (group "/"))
                (1 "|") (2 "|")))))

;;;###autoload
(add-to-list 'auto-mode-alist `(,(rx ".hx" eol) . haxe-mode))

(provide 'haxe-mode)

;;; haxe-mode.el ends here
