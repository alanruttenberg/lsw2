(add-to-list 'load-path "~/repos/slime/")
(require 'slime-autoloads)

(defun sldb-toggle-details-or-turn-of-line-truncation ()
  (interactive)
  (if (not (ignore-errors (sldb-frame-number-at-point)))
      (setq truncate-lines (not truncate-lines) word-wrap (not word-wrap) wrap-prefix "  ")
    (sldb-toggle-details)))

(setq slime-inspector-limit 1500)
(setq slime-repl-history-size 10000)
      
(add-hook 'slime-connected-hook 'slime-custom-keys)

(defun slime-custom-keys ()
  (define-key sldb-mode-map "t" 'sldb-toggle-details-or-turn-of-line-truncation))

(setq slime-contribs 
      '(slime-repl
	slime-autodoc
	slime-editing-commands
	slime-fancy-inspector
	slime-fancy-trace
	slime-mdot-fu
	slime-macrostep
	slime-presentations
	slime-scratch
	slime-references
	slime-package-fu
	slime-fontifying-fu
	slime-trace-dialog 
	slime-asdf 
	slime-hyperdoc 
	slime-indentation 
	slime-allsymbols-company
	slime-mrepl
	slime-repl-ansi-color
	))


(defun slime-init-abcl ()
  (slime-trace-dialog-init)
  (slime-repl-init)
  (slime-autodoc-init)
  (slime-editing-commands-init)
  (slime-fancy-inspector-init)
  (slime-fancy-trace-init)
  (slime-presentations-init)
  (slime-scratch-init)
  (slime-references-init)
  (slime-fontifying-fu-init)
  )

(setq slime-lisp-implementations 
      '((lsw ("~/repos/lsw2/bin/lsw") :init-function slime-init-abcl)
	))

;; If we eval something directly in inferior lisp buffer, print
;; there. Otherwise let slime output to standard output
(defun direct-slime-inferior-output ()
  (set-process-filter
   (slime-inferior-process)
   (lambda (process string)
     (if (eq (current-buffer) (process-buffer (slime-inferior-process)))
	  (comint-output-filter (slime-inferior-process) string)
       (slime-write-string string)))))

(add-hook 'slime-connected-hook 'direct-slime-inferior-output)



