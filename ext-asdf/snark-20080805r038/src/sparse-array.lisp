;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark-sparse-array -*-
;;; File: sparse-array.lisp
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
;;; Portions created by the Initial Developer are Copyright (C) 1981-2006.
;;; All Rights Reserved.
;;;
;;; Contributor(s): Mark E. Stickel <stickel@ai.sri.com>.

(in-package :snark-sparse-array)

;;; functions in this file should not depend on implementation details of sparse-vectors

#+cormanlisp
(defun (setf sparef1) (value sparse-vector index)
  (declare (ignore value sparse-vector index))
  (unimplemented))

#+cormanlisp
(defun (setf sparse-matrix-row) (value sparse-matrix index)
  (declare (ignore value sparse-matrix index))
  (unimplemented))

#+cormanlisp
(defun (setf sparse-matrix-column) (value sparse-matrix index)
  (declare (ignore value sparse-matrix index))
  (unimplemented))

;;; ****s* snark-sparse-array/sparse-matrix
;;; NAME
;;;   sparse-matrix structure
;;;   sparse-matrix type
;;; SOURCE

(defstruct (sparse-matrix
            (:constructor make-sparse-matrix0 (default-value boolean rows columns))
            (:print-function print-sparse-matrix3)
            (:copier nil))
  (default-value nil :read-only t)
  (boolean nil :read-only t)
  (rows nil :read-only t)
  (columns nil :read-only t))
;;; ***

;;; ****f* snark-sparse-array/make-sparse-matrix
;;; USAGE
;;;   (make-sparse-matrix &key boolean default-value rows columns)
;;; RETURN VALUE
;;;   sparse-matrix
;;; SOURCE

(defun make-sparse-matrix (&key boolean default-value (rows t rows-supplied) (columns t columns-supplied))
  (when boolean
    (unless (null default-value)
      (error "Default-value must be NIL for Boolean sparse-arrays.")))
  (let ((rows (and (or (not columns) (if rows-supplied rows (not columns-supplied)))
                   (make-sparse-vector)))
        (columns (and (or (not rows) (if columns-supplied columns (not rows-supplied)))
                      (make-sparse-vector))))
    (let ((sparse-matrix (make-sparse-matrix0 default-value boolean rows columns)))
      (when rows
        (setf (sparse-vector-type rows) `(rows ,sparse-matrix)))
      (when columns
        (setf (sparse-vector-type columns) `(columns ,sparse-matrix)))
      sparse-matrix)))
;;; ***

;;; ****f* snark-sparse-array/sparse-matrix-p
;;; USAGE
;;;   (sparse-matrix-p x)
;;; RETURN VALUE
;;;   true if x if a sparse-matrix, false otherwise
;;; SOURCE

      ;;sparse-matrix-p is defined by the sparse-matrix defstruct
;;; ***

;;; ****f* snark-sparse-array/sparse-matrix-boolean
;;; USAGE
;;;   (sparse-matrix-boolean sparse-matrix)
;;; RETURN VALUE
;;;   true if x is a boolean sparse-matrix, false otherwise
;;; SOURCE
      ;;sparse-matrix-boolean is defined as a slot in the sparse-matrix structure
;;; ***

;;; ****f* snark-sparse-array/sparse-matrix-default-value
;;; USAGE
;;;   (sparse-matrix-boolean sparse-matrix)
;;; RETURN VALUE
;;;   the default-value for unstored entries of sparse-matrix
;;; SOURCE
      ;;sparse-matrix-default-value is defined as a slot in the sparse-matrix structure
;;; ***

;;; ****f* snark-sparse-array/sparse-matrix-rows
;;; USAGE
;;;   (sparse-matrix-rows sparse-matrix)
;;; RETURN VALUE
;;;   sparse-vector of rows indexed by row-numbers or
;;;   nil if sparse-matrix is stored only by columns
;;; SOURCE

      ;;sparse-matrix-rows is defined as a slot in the sparse-matrix structure
;;; ***

;;; ****f* snark-sparse-array/sparse-matrix-columns
;;; USAGE
;;;   (sparse-matrix-columns sparse-matrix)
;;; RETURN VALUE
;;;   sparse-vector of columns indexed by column-numbers or
;;;   nil if sparse-matrix is stored only by rows
;;; SOURCE

      ;;sparse-matrix-columns is defined as a slot in the sparse-matrix structure
;;; ***

;;; ****f* snark-sparse-array/sparse-matrix-count
;;; USAGE
;;;   (sparse-matrix-count sparse-matrix)
;;; RETURN VALUE
;;;   integer number of entries in sparse-matrix
;;; SOURCE

(defun sparse-matrix-count (sparse-matrix)
  (let ((n 0))
    (prog->
      (map-sparse-vector
       (or (sparse-matrix-rows sparse-matrix) (sparse-matrix-columns sparse-matrix)) ->* v)
      (incf n (sparse-vector-count v)))
    n))
;;; ***

;;; ****if* snark-sparse-array/sparef2
;;; USAGE
;;;   (sparef2 sparse-matrix row-index col-index)
;;; NOTES
;;;   (sparef sparse-matrix row-index col-index) macroexpands to this
;;; SOURCE

(defun sparef2 (sparse-matrix row-index col-index)
  (let ((rows (sparse-matrix-rows sparse-matrix)))
    (if rows
        (let ((row (sparef rows row-index)))
          (if row (sparef row col-index) (sparse-matrix-default-value sparse-matrix)))
        (let ((col (sparef (sparse-matrix-columns sparse-matrix) col-index)))
          (if col (sparef col row-index) (sparse-matrix-default-value sparse-matrix))))))
;;; ***

;;; ****f* snark-sparse-array/sparse-matrix-row
;;; USAGE
;;;   (sparse-matrix-row sparse-matrix index)
;;;   (setf (sparse-matrix-row sparse-matrix index) sparse-vector)
;;;   (setf (sparse-matrix-row sparse-matrix index) nil)
;;;   (setf (sparse-matrix-row sparse-matrix index) t)
;;; RETURN VALUE
;;;   sparse-vector or nil
;;; DESCRIPTION
;;;   (sparse-matrix-row sparse-matrix index) returns
;;;   the index-th row of sparse-matrix if it exists, nil otherwise.
;;;
;;;   (setf (sparse-matrix-row sparse-matrix index) sparse-vector) replaces
;;;   the index-th row of sparse-matrix by sparse-vector.
;;;
;;;   (setf (sparse-matrix-row sparse-matrix index) nil) deletes
;;;   the index-th row of sparse-matrix.
;;;
;;;   (setf (sparse-matrix-row sparse-matrix index) t) returns
;;;   the index-th row of sparse-matrix if it exists
;;;   or adds and returns a new one otherwise.
;;;   It is equivalent to
;;;   (or (sparse-matrix-row sparse-matrix index)
;;;       (setf (sparse-matrix-row sparse-matrix index)
;;;             (make-sparse-vector :boolean (sparse-matrix-boolean sparse-matrix)
;;;                                 :default-value (sparse-matrix-default-value sparse-matrix))))
;;; SOURCE

(defun sparse-matrix-row (sparse-matrix index)
  (let ((rows (sparse-matrix-rows sparse-matrix)))
    (and rows (sparef rows index))))

(defun (setf sparse-matrix-row) (value sparse-matrix index)
  (let ((rows (sparse-matrix-rows sparse-matrix)))
    (if rows
        (setf (sparef rows index) value)
        (error "No row vectors for sparse-matrix ~A." sparse-matrix))))
;;; ***

;;; ****f* snark-sparse-array/sparse-matrix-column
;;; USAGE
;;;   (setf (sparse-matrix-column sparse-matrix index) sparse-vector)
;;;   (setf (sparse-matrix-column sparse-matrix index) nil)
;;;   (setf (sparse-matrix-column sparse-matrix index) t)
;;; RETURN VALUE
;;;   sparse-vector or nil
;;; DESCRIPTION
;;;   Defined analogously to sparse-matrix-row.
;;; SOURCE

(defun sparse-matrix-column (sparse-matrix index)
  (let ((cols (sparse-matrix-columns sparse-matrix)))
    (and cols (sparef cols index))))

(defun (setf sparse-matrix-column) (value sparse-matrix index)
  (let ((cols (sparse-matrix-columns sparse-matrix)))
    (if cols
        (setf (sparef cols index) value)
        (error "No column vectors for sparse-matrix ~A." sparse-matrix))))
;;; ***

;;; ****if* snark-sparse-array/add-sparse-matrix-row-or-column
;;; USAGE
;;;   (add-sparse-matrix-row-or-column rows-or-cols index new-row-or-col)
;;; SOURCE

(defun add-sparse-matrix-row-or-column (rows-or-cols index new-row-or-col)
  (let ((type (sparse-vector-type rows-or-cols))
        sparse-matrix cols-or-rows)
    (ecase (first type)
      (rows
       (setf sparse-matrix (second type))
       (setf cols-or-rows (sparse-matrix-columns sparse-matrix))
       (setf type `(row ,sparse-matrix ,index)))
      (columns
       (setf sparse-matrix (second type))
       (setf cols-or-rows (sparse-matrix-rows sparse-matrix))
       (setf type `(column ,sparse-matrix ,index))))
    (unless (eql 0 (sparse-vector-count new-row-or-col))
      (when cols-or-rows
        (prog->
          (map-sparse-vector-with-indexes new-row-or-col ->* value index2)
          (sparse-vector-setter
           value (or (sparef cols-or-rows index2) (setf (sparef cols-or-rows index2) t)) index))))
    (setf (sparse-vector-type new-row-or-col) type)
    (sparse-vector-setter new-row-or-col rows-or-cols index)))
;;; ***

;;; ****if* snark-sparse-array/delete-sparse-matrix-row-or-column
;;; USAGE
;;;   (delete-sparse-matrix-row-or-column rows-or-cols index &optional keep)
;;; SOURCE

(defun delete-sparse-matrix-row-or-column (rows-or-cols index &optional keep)
  ;; removes indexth sparse-vector from rows-or-cols
  ;; and deletes its entries from cols-or-rows
  ;; but leaves contents of removed sparse-vector intact
  (let ((sparse-vector (sparef rows-or-cols index)))
    (when sparse-vector
      (unless (eql 0 (sparse-vector-count sparse-vector))
        (let ((cols-or-rows (let ((type (sparse-vector-type rows-or-cols)))
                              (ecase (first type)
                                (rows (sparse-matrix-columns (second type)))
                                (columns (sparse-matrix-rows (second type))))))
              (default-value (sparse-vector-default-value sparse-vector)))
          (prog->
            (map-sparse-vector-indexes-only sparse-vector ->* index2)
            (sparse-vector-setter default-value (sparef cols-or-rows index2) index))))
      (setf (sparse-vector-type sparse-vector) nil)
      (unless keep
        (sparse-vector-setter nil rows-or-cols index)))))
;;; ***

;;; ****if* snark-sparse-array/(setf_sparef1)
;;; USAGE
;;;   (setf (sparef1 sparse-vector index) value)
;;; SOURCE

(defun (setf sparef1) (value sparse-vector index)
  (let ((type (sparse-vector-type sparse-vector)))
    (if (null type)
        (sparse-vector-setter value sparse-vector index)
        (ecase (first type)
          (row
           (let ((matrix (second type))
                 (row-index (third type)))
             (if (eql value (sparse-vector-default-value sparse-vector))
                 (let ((col (sparse-matrix-column matrix index)))
                   (when col
                     (sparse-vector-setter value col row-index)))
                 (when (sparse-matrix-columns matrix)
                   (sparse-vector-setter value (setf (sparse-matrix-column matrix index) t) row-index))))
           (sparse-vector-setter value sparse-vector index))
          (column
           (let ((matrix (second type))
                 (col-index (third type)))
             (if (eql value (sparse-vector-default-value sparse-vector))
                 (let ((row (sparse-matrix-row matrix index)))
                   (when row
                     (sparse-vector-setter value row col-index)))
                 (when (sparse-matrix-rows matrix)
                   (sparse-vector-setter value (setf (sparse-matrix-row matrix index) t) col-index))))
           (sparse-vector-setter value sparse-vector index))
          ((rows columns)
           (cond
            ((null value)
             (delete-sparse-matrix-row-or-column sparse-vector index nil))
            ((eq t value)
             (or (sparef sparse-vector index)
                 (progn
                   (let ((matrix (second type)))
                     (setf value (make-sparse-vector
                                  :default-value (sparse-matrix-default-value matrix)
                                  :boolean (sparse-matrix-boolean matrix))))
                   (delete-sparse-matrix-row-or-column sparse-vector index t)
                   (add-sparse-matrix-row-or-column sparse-vector index value))))
            (t
             (let ((matrix (second type)))
               (cl:assert (and (sparse-vector-p value)
                               (null (sparse-vector-type value))
                               (if (sparse-vector-boolean value)
                                   (sparse-vector-boolean matrix)
                                   (and (not (sparse-vector-boolean matrix))
                                        (eql (sparse-vector-default-value value)
                                             (sparse-vector-default-value matrix)))))))
             (delete-sparse-matrix-row-or-column sparse-vector index t)
             (add-sparse-matrix-row-or-column sparse-vector index value))))))))
;;; ***

;;; ****if* snark-sparse-array/(setf_sparef2)
;;; USAGE
;;;   (setf (sparef2 sparse-matrix row-index col-index) value)
;;; SOURCE

(defun (setf sparef2) (value sparse-matrix row-index col-index)
  (let ((rows (sparse-matrix-rows sparse-matrix))
        (cols (sparse-matrix-columns sparse-matrix)))
    (cond
     ((eql value (sparse-matrix-default-value sparse-matrix))
      (let ((col (and cols (sparef cols col-index))))
        (when col
          (sparse-vector-setter value col row-index)))
      (let ((row (and rows (sparef rows row-index))))
        (if row
            (sparse-vector-setter value row col-index)
            value)))
     (t
      (when cols
        (sparse-vector-setter value (setf (sparse-matrix-column sparse-matrix col-index) t) row-index))
      (if rows
          (sparse-vector-setter value (setf (sparse-matrix-row sparse-matrix row-index) t) col-index)
          value)))))
;;; ***

;;; ****f* snark-sparse-array/map-sparse-matrix
;;; USAGE
;;;   (map-sparse-matrix function sparse-matrix)
;;; RETURN VALUE
;;;   nil
;;; DESCRIPTION
;;;   The map-sparse-matrix function applies its unary-function argument
;;;   to each value in sparse-matrix.
;;; SEE ALSO
;;;   map-sparse-matrix-with-indexes
;;;   map-sparse-matrix-indexes-only
;;; SOURCE

(defun map-sparse-matrix (function sparse-matrix)
  (let ((rows (sparse-matrix-rows sparse-matrix)))
    (if rows
        (prog->
          (map-sparse-vector rows ->* row)
          (map-sparse-vector row ->* value)
          (funcall function value))
        (prog->
          (map-sparse-vector (sparse-matrix-columns sparse-matrix) ->* col)
          (map-sparse-vector col ->* value)
          (funcall function value)))))
;;; ***

;;; ****f* snark-sparse-array/map-sparse-matrix-with-indexes
;;; USAGE
;;;   (map-sparse-matrix-with-indexes function sparse-matrix)
;;; RETURN VALUE
;;;   nil
;;; DESCRIPTION
;;;   The map-sparse-matrix-with-indexes function applies its ternary-function argument
;;;   to each value, row-index, and column-index in sparse-matrix.
;;; SEE ALSO
;;;   map-sparse-matrix
;;;   map-sparse-matrix-indexes-only
;;; SOURCE

(defun map-sparse-matrix-with-indexes (function sparse-matrix)
  (let ((rows (sparse-matrix-rows sparse-matrix)))
    (if rows
        (prog->
          (map-sparse-vector-with-indexes rows ->* row row-index)
          (map-sparse-vector-with-indexes row ->* value col-index)
          (funcall function value row-index col-index))
        (prog->
          (map-sparse-vector-with-indexes (sparse-matrix-columns sparse-matrix) ->* col col-index)
          (map-sparse-vector-with-indexes col ->* value row-index)
          (funcall function value row-index col-index)))))
;;; ***

;;; ****f* snark-sparse-array/map-sparse-matrix-indexes-only
;;; USAGE
;;;   (map-sparse-matrix-indexes-only function sparse-matrix)
;;; RETURN VALUE
;;;   nil
;;; DESCRIPTION
;;;   The map-sparse-matrix-indexes-only function applies its binary-function argument
;;;   to each row-index and column-index in sparse-matrix.
;;; SEE ALSO
;;;   map-sparse-matrix
;;;   map-sparse-matrix-with-indexes
;;; SOURCE

(defun map-sparse-matrix-indexes-only (function sparse-matrix)
  (let ((rows (sparse-matrix-rows sparse-matrix)))
    (if rows
        (prog->
          (map-sparse-vector-with-indexes rows ->* row row-index)
          (map-sparse-vector-indexes-only row ->* col-index)
          (funcall function row-index col-index))
        (prog->
          (map-sparse-vector-with-indexes (sparse-matrix-columns sparse-matrix) ->* col col-index)
          (map-sparse-vector-indexes-only col ->* row-index)
          (funcall function row-index col-index)))))
;;; ***

;;; ****if* snark-sparse-array/print-sparse-vector3
;;; USAGE
;;;   (print-sparse-vector3 sparse-vector stream depth)
;;; NOTES
;;;   specified as print-function in the sparse-vector defstruct
;;; SOURCE

(defun print-sparse-vector3 (sparse-vector stream depth)
  (declare (ignore depth))
  (print-unreadable-object (sparse-vector stream :type t :identity t)
    (princ "count " stream)
    (princ (sparse-vector-count sparse-vector) stream)))
;;; ***

;;; ****if* snark-sparse-array/print-sparse-matrix3
;;; USAGE
;;;   (print-sparse-matrix3 sparse-matrix stream depth)
;;; NOTES
;;;   specified as print-function in the sparse-matrix defstruct
;;; SOURCE

(defun print-sparse-matrix3 (sparse-matrix stream depth)
  (declare (ignore depth))
  (print-unreadable-object (sparse-matrix stream :type t :identity t)
    (let ((rows (sparse-matrix-rows sparse-matrix)))
      (princ (if rows (sparse-vector-count rows) "?") stream))
    (princ " rows" stream)
    (princ " * " stream)
    (let ((cols (sparse-matrix-columns sparse-matrix)))
      (princ (if cols (sparse-vector-count cols) "?") stream))
    (princ " cols" stream)))
;;; ***

;;; sparse-array.lisp EOF
