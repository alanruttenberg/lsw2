;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: term-memory.lisp
;;; The contents of this file are subject to the Mozilla Public License
;;; Version 1.1 (the "License"); you may not use this file except in
;;; compliance with the License. You may obtain a copy of the License at
;;; http://www.mozilla.org/MPL/
;;;
;;; Software distributed under the License is distributed on an "AS IS"
;;; basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
;;; License for the specific language governing rights and limitations
;;; under the License.
;;;
;;; The Original Code is SNARK.
;;; The Initial Developer of the Original Code is SRI International.
;;; Portions created by the Initial Developer are Copyright (C) 1981-2012.
;;; All Rights Reserved.
;;;
;;; Contributor(s): Mark E. Stickel <stickel@ai.sri.com>.

(in-package :snark)

(defvar *term-memory*)

(defstruct (term-memory-entry
	     (:include path-index-entry)
	     (:conc-name :tme-)
	     (:copier nil))
  (number (nonce) :read-only t)
  (rows-containing-atom-positively nil)
  (rows-containing-atom-negatively nil)
  (rows-containing-paramodulatable-equality nil)
  (rows-containing-term nil)
  (rewrites nil)
  size
  depth
  mindepth)

(defstruct (term-memory
	     (:conc-name :tm-)
	     (:constructor make-term-memory0)
	     (:copier nil))
  (retrieve-generalization-calls 0)		;number of generalization retrieval calls
  (retrieve-generalization-count 0)
  (retrieve-instance-calls 0)			;    "     instance          "
  (retrieve-instance-count 0)
  (retrieve-unifiable-calls 0)			;    "     unifiable         "
  (retrieve-unifiable-count 0)
  (retrieve-variant-calls 0)			;    "     variant           "
  (retrieve-variant-count 0)
  (retrieve-all-calls 0)			;    "     all               "
  (retrieve-all-count 0)
  )

(defun make-term-memory-entry1 (term)
  (make-term-memory-entry
   :term term
   :size (size term)
   :depth (depth term)
   :mindepth (mindepth term)))

(defun make-term-memory (&key indexing-method depth-limit make-printable-nodes-p)
  (declare (ignore indexing-method depth-limit make-printable-nodes-p))
  (make-path-index :entry-constructor #'make-term-memory-entry1)
  (make-trie-index :entry-constructor #'make-term-memory-entry1)
  (setf *term-memory* (make-term-memory0))
  *term-memory*)

(defun term-memory-entry (term)
;;(path-index-entry term)
  (nth-value 1 (tm-store term))
  )

(defun some-term-memory-entry (term)
  (some-path-index-entry term))

(defun the-term-memory-entry (term)
  (the-path-index-entry term))

(defun tm-store (term)
;;(cl:assert (eql term (hash-term term)))
  (when (variable-p term)
    (error "STORING VARIABLE IN TERM MEMORY"))
  (let (entry)
    (cond
      ((setf entry (some-path-index-entry term))
       (cl:assert (eql term (tme-term entry)))
       (values term entry t))
      (t
       (setf entry (path-index-insert term))
       (cl:assert (eql term (tme-term entry)))
       (trie-index-insert term entry)
       (when (or (test-option51?) (test-option52?))
         (feature-vector-index-insert entry *feature-vector-term-index*))
       (values term entry)))))

(defun tm-remove-entry (entry)
  (let ((rowset (tme-rows-containing-term entry)))
    (when rowset
      (rowsets-delete-column rowset)
      (setf (tme-rows-containing-term entry) nil)))
  (let ((rowset (tme-rows-containing-atom-positively entry)))
    (when rowset
      (rowsets-delete-column rowset)
      (setf (tme-rows-containing-atom-positively entry) nil)))
  (let ((rowset (tme-rows-containing-atom-negatively entry)))
    (when rowset
      (rowsets-delete-column rowset)
      (setf (tme-rows-containing-atom-negatively entry) nil)))
  (path-index-delete (tme-term entry))
  (trie-index-delete (tme-term entry) entry)
  (when (or (test-option51?) (test-option52?))
    (feature-vector-index-delete entry *feature-vector-term-index*)))

(defun retrieve-generalization-entries (cc term &optional subst test)
  (when (test-option51?)
    (if (null test)
        (prog->
          (map-feature-vector-term-index-generalizations term subst ->* entry)
          (funcall cc entry))
        (prog->
          (map-feature-vector-term-index-generalizations term subst ->* entry)
          (funcall test entry ->nonnil test-value)
          (funcall cc entry test-value)))
    (return-from retrieve-generalization-entries))
  #-ignore (incf (tm-retrieve-generalization-calls *term-memory*))
  (if (null test)
      (prog->
        (map-trie-index :generalization term subst ->* entry)
        #-ignore (incf (tm-retrieve-generalization-count *term-memory*))
        (funcall cc entry))
      (prog->
        (map-trie-index :generalization term subst ->* entry)
        (funcall test entry ->nonnil test-value)
        #-ignore (incf (tm-retrieve-generalization-count *term-memory*))
        (funcall cc entry test-value))))

(defun retrieve-instance-entries (cc term &optional subst test)
  (when (test-option52?)
    (if (null test)
        (prog->
          (map-feature-vector-term-index-instances term subst ->* entry)
          (funcall cc entry))
        (prog->
          (map-feature-vector-term-index-instances term subst ->* entry)
          (funcall test entry ->nonnil test-value)
          (funcall cc entry test-value)))
    (return-from retrieve-instance-entries))
  #-ignore (incf (tm-retrieve-instance-calls *term-memory*))
  (cond
   ((and (ground-p term subst) (simply-indexed-p term subst))
    (if (null test)
        (prog->
          (map-trie-index :instance term subst ->* entry)
          #-ignore (incf (tm-retrieve-instance-count *term-memory*))
          (funcall cc entry))
        (prog->
          (map-trie-index :instance term subst ->* entry)
          (funcall test entry ->nonnil test-value)
          #-ignore (incf (tm-retrieve-instance-count *term-memory*))
          (funcall cc entry test-value))))
   (t
    (if (null test)
        (prog->
          (map-path-index-entries :instance term subst test ->* entry)
          #-ignore (incf (tm-retrieve-instance-count *term-memory*))
          (funcall cc entry))
        (prog->
          (map-path-index-entries :instance term subst test ->* entry test-value)
          #-ignore (incf (tm-retrieve-instance-count *term-memory*))
          (funcall cc entry test-value))))))

(defun retrieve-unifiable-entries (cc term &optional subst test)
  #-ignore (incf (tm-retrieve-unifiable-calls *term-memory*))
  (if (null test)
      (prog->
	(map-path-index-entries :unifiable term subst test ->* entry)
	#-ignore (incf (tm-retrieve-unifiable-count *term-memory*))
	(funcall cc entry))
      (prog->
	(map-path-index-entries :unifiable term subst test ->* entry test-value)
	#-ignore (incf (tm-retrieve-unifiable-count *term-memory*))
	(funcall cc entry test-value))))

(defun retrieve-resolvable-entries (cc atom &optional subst test)
  (unless (do-not-resolve atom)
    (retrieve-unifiable-entries cc atom subst test)))

(defun retrieve-paramodulatable-entries (cc term &optional subst test)
  (unless (do-not-paramodulate term)
    (retrieve-unifiable-entries cc term subst test)))

(defun retrieve-variant-entries (cc term &optional subst test)
  #-ignore (incf (tm-retrieve-variant-calls *term-memory*))
  (if (null test)
      (prog->
        (map-path-index-entries :variant term subst test ->* entry)
        #-ignore (incf (tm-retrieve-variant-count *term-memory*))
        (funcall cc entry))
      (prog->
        (map-path-index-entries :variant term subst test ->* entry test-value)
        #-ignore (incf (tm-retrieve-variant-count *term-memory*))
        (funcall cc entry test-value))))

(defun retrieve-all-entries (cc &optional test)
  #-ignore (incf (tm-retrieve-all-calls *term-memory*))
  (if (null test)
      (prog->
	(map-path-index-by-query t test ->* entry)
	#-ignore (incf (tm-retrieve-all-count *term-memory*))
	(funcall cc entry))
      (prog->
	(map-path-index-by-query t test ->* entry test-value)
	#-ignore (incf (tm-retrieve-all-count *term-memory*))
	(funcall cc entry test-value))))

(defun print-term-memory (&key terms nodes)
  (print-term-hash :terms nil :details nil)
  (print-feature-vector-row-index)
  (when (or (test-option51?) (test-option52?))
    (print-feature-vector-term-index))
  (print-path-index :terms terms :nodes nodes)
  (print-trie-index :terms terms :nodes nodes)
  (unless (eql 0 (tm-retrieve-variant-calls *term-memory*))
    (format t "~%; Retrieved ~:D variant term~:P in ~:D call~:P."
            (tm-retrieve-variant-count *term-memory*)
            (tm-retrieve-variant-calls *term-memory*)))
  (unless (eql 0 (tm-retrieve-generalization-calls *term-memory*))
    (format t "~%; Retrieved ~:D generalization term~:P in ~:D call~:P."
            (tm-retrieve-generalization-count *term-memory*)
            (tm-retrieve-generalization-calls *term-memory*)))
  (unless (eql 0 (tm-retrieve-instance-calls *term-memory*))
    (format t "~%; Retrieved ~:D instance term~:P in ~:D call~:P."
            (tm-retrieve-instance-count *term-memory*)
            (tm-retrieve-instance-calls *term-memory*)))
  (unless (eql 0 (tm-retrieve-unifiable-calls *term-memory*))
    (format t "~%; Retrieved ~:D unifiable term~:P in ~:D call~:P."
            (tm-retrieve-unifiable-count *term-memory*)
            (tm-retrieve-unifiable-calls *term-memory*)))
  (unless (eql 0 (tm-retrieve-all-calls *term-memory*))
    (format t "~%; Retrieved ~:D unrestricted term~:P in ~:D call~:P."
            (tm-retrieve-all-count *term-memory*)
            (tm-retrieve-all-calls *term-memory*))))

(defun tme-useless-p (entry)
  (and (eql 0 (sparse-vector-count (tme-rows-containing-term entry)))
       (eql 0 (sparse-vector-count (tme-rows-containing-atom-positively entry)))
       (eql 0 (sparse-vector-count (tme-rows-containing-atom-negatively entry)))
       (null (tme-rows-containing-paramodulatable-equality entry))
       (null (tme-rewrites entry))))

(defmacro rows-containing-atom-positively (atom)
  `(tme-rows-containing-atom-positively
    (term-memory-entry ,atom)))

(defmacro rows-containing-atom-negatively (atom)
  `(tme-rows-containing-atom-negatively
    (term-memory-entry ,atom)))

(defmacro rows-containing-paramodulatable-equality (equality)
  `(tme-rows-containing-paramodulatable-equality
    (term-memory-entry ,equality)))

(defmacro rows-containing-term (term)
  `(tme-rows-containing-term
    (term-memory-entry ,term)))

(defmacro rewrites (term)
  `(tme-rewrites
    (term-memory-entry ,term)))

(defun insert-into-rows-containing-term (row term)
  (let ((e (term-memory-entry term)))
    (rowset-insert row (or (tme-rows-containing-term e)
                           (setf (tme-rows-containing-term e) (make-rowset))))))

(defun insert-into-rows-containing-atom-positively (row atom)
  (let ((e (term-memory-entry atom)))
    (rowset-insert row (or (tme-rows-containing-atom-positively e)
                           (setf (tme-rows-containing-atom-positively e) (make-rowset))))))

(defun insert-into-rows-containing-atom-negatively (row atom)
  (let ((e (term-memory-entry atom)))
    (rowset-insert row (or (tme-rows-containing-atom-negatively e)
                           (setf (tme-rows-containing-atom-negatively e) (make-rowset))))))

;;; term-memory.lisp EOF
