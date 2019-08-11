(require 'f)

(defvar haxe-mode-support-path
  (f-dirname load-file-name))

(defvar haxe-mode-features-path
  (f-parent haxe-mode-support-path))

(defvar haxe-mode-root-path
  (f-parent haxe-mode-features-path))

(add-to-list 'load-path haxe-mode-root-path)

;; Ensure that we don't load old byte-compiled versions
(let ((load-prefer-newer t))
  (require 'haxe-mode)
  (require 'espuds)
  (require 'ert))

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
