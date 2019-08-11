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
  (font-lock-fontify-buffer)
  (setq-local comment-start "// ")
  (setq-local comment-start-skip (rx "//" (* (or "//" whitespace)))))

(provide 'haxe-mode)

;;; haxe-mode.el ends here
