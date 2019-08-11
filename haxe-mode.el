;;; haxe-mode.el --- Major mode for Haxe

;;; Version: 0.0.1

;;; Commentary:
;;

(defconst haxe-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?' "\"" table)     ;; single quote string
    (modify-syntax-entry ?\" "\"" table)    ;; double quote string
    (modify-syntax-entry ?/ ". 124b" table) ;; /* comment, // comment
    (modify-syntax-entry ?* ". 23" table)   ;; /* comment
    (modify-syntax-entry ?\n "> b" table)   ;; // comment eol
    table)
  "Syntax table for haxe-mode.")

(defconst haxe-mode-font-lock-keywords
  (let* ((keywords '(abstract cast class dynamic enum extends
                     extern false final final function implements
                     import in inline interface macro new null
                     operator overload override package public
                     static this true typedef untyped var))
         (controls '(break case catch continue default do else
                     for if return switch throw try while))
         (keyword-strings (mapcar 'symbol-name (append keywords controls)))
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
  "Font lock configuration for haxe-mode.")

(defconst haxe-mode-keywords
  '(abstract break case cast catch class continue default do
             dynamic else enum extends extern false final for function if
             implements import in inline interface macro new null operator
             overload override package private public return static switch
             this throw true try typedef untyped using var while)
  "Keywords for haxe-mode.")

(define-derived-mode haxe-mode prog-mode "Haxe"
  "A major mode for editing Haxe files."
  :syntax-table haxe-mode-syntax-table
  (setq-local comment-start "// ")
  (setq-local comment-start-skip (rx "//" (* (or "//" whitespace))))
  (setq-local font-lock-defaults '((haxe-mode-font-lock-keywords)))
  (font-lock-fontify-buffer))

(provide 'haxe-mode)

;;; haxe-mode.el ends here
