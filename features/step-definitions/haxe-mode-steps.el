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

(When (rx bol (group (*? anything)) (? " ") "at "
          (group (or "point" "line")) " "
          (group (+ digit)) eol)
  (lambda (step kind line)
    (if (equal "point" kind)
        (goto-char (string-to-number line))
      (goto-line (string-to-number line)))
    (unless (s-blank? step)
      (When step))))

(Then "^I should have$"
  (lambda (body)
    (should (equal body (buffer-string)))))

(Then (rx "expect " (group "font-lock-" (+ anything)) eol)
  (lambda (font-face)
    (font-lock-fontify-buffer)
    (should (equal font-face (symbol-name (get-text-property (point) 'face))))))
