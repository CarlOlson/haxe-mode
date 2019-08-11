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

(When (rx bol (group (* anything)) " at line " (group (+ digit)) eol)
  (lambda (step line)
    (goto-line (string-to-number line))
    (When step)))

(Then "^I should have$"
  (lambda (body)
    (should (s-equals? body (buffer-string)))))
