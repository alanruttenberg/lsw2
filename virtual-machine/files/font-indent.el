(defface alanrs-font-lock-function-name-face
  '((t (:inverse-video t :bold t)))
  "Font Lock mode face used to highlight function names."
  :group 'font-lock-highlighting-faces)

(defface logic-keyword-face
  '((t (:foreground "OliveDrab4")))
  "Font Lock mode face used to highlight function names."
  :group 'font-lock-highlighting-faces)

(defface logic-variable-face
  '((t (:foreground "chocolate")))
  "Font Lock mode face used to highlight function names."
  :group 'font-lock-highlighting-faces)

(defun alanr-lisp-mode-hook ()
  (set (make-local-variable lisp-indent-function) 'common-lisp-indent-function)
  (font-lock-add-keywords
   'lisp-mode
   '(("(in-package\\s-+\\([:\"']?\\sw+\\)"  1 font-lock-warning-face t)))
  (font-lock-add-keywords
   'lisp-mode
   '(("(def[A-Za-z0-9+-]+ \\([A-Za-z0-9+-]+\\)" 1 font-lock-function-name-face t)))
 (font-lock-add-keywords
  'lisp-mode
  '(("\\(:\\(\\(iff\\)\\|\\(implies\\)\\|\\(and\\)\\|\\(or\\)\\|\\(not\\)\\|\\(distinct\\)\\|\\(forall\\)\\|\\(exists\\)\\)\\)" 1 'logic-keyword-face t)))
  (font-lock-add-keywords
   'lisp-mode
   '(("[ (]\\([?][A-Za-z0-9-_]+\\)" 1 'logic-variable-face t)))
  (turn-on-font-lock))

;; lsw
(put 'object-property 'common-lisp-indent-function 1)
(put 'datatype-property 'common-lisp-indent-function 1)
(put 'individual 'common-lisp-indent-function 1)
(put 'class 'common-lisp-indent-function 1)
(put 'class    'doc-string-elt 2)
(put 'object-property 'doc-string-elt 2)
(put 'datatype-property 'doc-string-elt 2)
(put 'individual 'doc-string-elt 2)
(put 'with-ontology 'common-lisp-indent-function 2)
(put 'each-axiom  'common-lisp-indent-function 2)
(put ':select 'common-lisp-indent-function 2)

(put ':forall 'common-lisp-indent-function 1)
(put ':exists 'common-lisp-indent-function 1)
(put ':not 'common-lisp-indent-function 1)
(put ':implies 'common-lisp-indent-function 2)
(put ':iff 'common-lisp-indent-function 2)
(put 'def-expect-provable 'common-lisp-indent-function (get 'defmacro 'common-lisp-indent-function))
(put 'def-expect-not-entailed 'common-lisp-indent-function (get 'defmacro 'common-lisp-indent-function))

;; lsw

(defun lisp-font-lock-syntactic-face-function (state)
  (if (nth 3 state)
      (if (and (or (eq (nth 0 state) 1) (eq (nth 0 state) 2)) ;;; <--- patch here, so that 1 level nested docstring (e.g. defun in progn work - but more for lsw.
       ;; This might be a docstring.
       (save-excursion
 (let ((n 0))
   (goto-char (nth 8 state))
   (condition-case nil
       (while (and (not (bobp))
   (progn (backward-sexp 1) (setq n (1+ n)))))
     (scan-error nil))
   (when (> n 0)
     (let ((sym (intern-soft
 (buffer-substring
  (point) (progn (forward-sexp 1) (point))))))
       (eq n (or (get sym 'doc-string-elt) 3)))))))
  font-lock-doc-face
font-lock-string-face)
    font-lock-comment-face))
(provide 'font-indent)
