;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Given "^I am in a haxe buffer$"
  (lambda ()
    (let ((buffer-name "haxe.hx"))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name))
      (switch-to-buffer
       (get-buffer-create buffer-name))
      (haxe-mode))))

(And "^I insert$"
  (lambda (body)
    (insert body)))

(When (rx "I '" (group (+ (or alphanumeric "-"))))
  (lambda (command)
    (call-interactively (intern command))))

(When "^I indent$" 'haxe-indent-line)

(When (rx bol (group (*? any)) (? " ") "at "
          (group (or "point" "line" "column")) " "
          (group (+ digit)) eol)
  (lambda (step kind value)
    (let ((inhibit-message t))
      (pcase kind
        ("point"
         (goto-char (string-to-number value)))
        ("column"
         (beginning-of-line)
         (right-char (string-to-number value)))
        ("line"
         (goto-line (string-to-number value)))))
    (unless (s-blank? step)
      (When step))))

(Then "^I should have$"
  (lambda (body)
    (should (equal body (buffer-string)))))

(Then (rx "expect " (group "font-lock-" (+ any)) eol)
  (lambda (font-face)
    (font-lock-fontify-buffer)
    (should (equal font-face (symbol-name (get-text-property (point) 'face))))))

(Then (rx "expect " (group (or "point" "column" "line")) (or " at " " to ") (group (+ digit)))
  (lambda (kind value)
    (pcase kind
      ("point"
       (should (equal (string-to-number value) (point))))
      ("column"
       (should (equal (string-to-number value) (current-column))))
      ("line"
       (should (equal (string-to-number value) (line-number-at-pos)))))))

(Then (rx (group "expect " (+ any)) " at indent + " (group (+ digit)))
  (lambda (prefix value)
    (Then (format "%s at %s" prefix (+ (current-indentation)
                                       (string-to-number value))))))

(Then (rx "expect indent level " (group (+ digit)))
  (lambda (value)
    (should (equal (* haxe-mode-indent-level
                      (string-to-number value))
                   (current-indentation)))))
