;;; haxe-mode.el --- Major mode for Haxe

;;; Version: 0.0.1

;;; Commentary:
;;

(define-derived-mode haxe-mode fundamental-mode "Haxe"
  "A major mode for editing Haxe files."
  (setq-local comment-start "// ")
  (setq-local comment-start-skip (rx "//" (* (or "//" whitespace)))))

(provide 'haxe-mode)

;;; haxe-mode.el ends here
