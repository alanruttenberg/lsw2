;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: feature-vector-trie.lisp
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

;;; feature vector tries are indexed by keys in ascending value
;;; where each key combines a feature number and its value

(definline fv-trie-key (feature-number feature-value)
  (+ (* (+ $fv-maximum-feature-value 1) feature-number) feature-value))

(definline fv-trie-key-feature (key)
  (nth-value 0 (floor key (+ $fv-maximum-feature-value 1))))

(definline fv-trie-key-value (key)
  (mod key (+ $fv-maximum-feature-value 1)))

(defun map-fv-trie<= (function trie keys)
  (labels
    ((mfvt (node keys done)
       (unless done
         (let ((d (trie-node-data node)))
           (when d
             (funcall function d))))
       (when keys
         (prog->
           (rest keys -> r)
           (mfvt node r t)
           (trie-node-branches node ->nonnil b)
           (first keys -> key)
           ;; map over subtries for key-feature = 1 ... key-feature = key-value
           (+ key (- 1 (fv-trie-key-value key)) -> key1)
           (cond
            ((= key1 key)
             (sparef b key ->nonnil node)
             (mfvt node r nil))
            (t
             (map-sparse-vector b :min key1 :max key ->* node)
             (mfvt node r nil)))))))
    (mfvt (trie-top-node trie) keys nil)))

(defun map-fv-trie>= (function trie keys)
  (labels
    ((mfvt (node keys)
       (if (null keys)
           (map-trie function node)
           (prog->
             (trie-node-branches node ->nonnil b)
             (rest keys -> r)
             (first keys -> key)
             (- key (fv-trie-key-value key) -> key0)
             (map-sparse-vector-with-indexes b :max (+ key0 $fv-maximum-feature-value) ->* node k)
             (cond
              ((< k key0)
               (mfvt node keys))
              ((>= k key)
               (mfvt node r)))))))
    (mfvt (trie-top-node trie) keys)))

;;; feature-vector-trie.lisp EOF
