;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark-infix-reader -*-
;;; File: infix-reader.lisp
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
;;; Portions created by the Initial Developer are Copyright (C) 1981-2011.
;;; All Rights Reserved.
;;;
;;; Contributor(s): Mark E. Stickel <stickel@ai.sri.com>.

(in-package :snark-infix-reader)

;;; no operator should be declared to be both infix and postfix
;;; to ease parsing as in ISO Prolog standard

;;; <identifier> = <ordinary-char>+ (but first character cannot be a digit)
;;; <number> = [<sign>] <digit>+ <decimal-point> <digit>+ for floats 
;;;            [<sign>] <digit>+ <forward-slash> <digit>+ for ratios
;;;            [<sign>] <digit>+                          for integers

(definline ordinary-char-p (char)
  (or (alpha-char-p char)
      (digit-char-p char)
      (eql #\_ char)
      (eql #\? char)					;for SNARK variables
      (eql #\$ char)))					;for builtins

(definline separator-char-p (char)
  (or (eql #\, char)					;comma is not an operator
      (eql #\( char)
      (eql #\) char)
      (eql #\[ char)
      (eql #\] char)
      (eql #\. char)))					;dot is not an operator

(definline whitespace-char-p (char)
  (or (eql #\space char)
      (eql #\tab char)
      (eql #\newline char)
      (eql #\return char)
      (eql #\linefeed char)
      (eql #\page char)))

(definline quotation-char-p (char)
  (or (eql #\" char)
      (eql #\' char)))

(definline comment-char-p (char)
  (eql #\% char))

(defun tokenize1 (stream &key (case :preserve) (upper-case-var-prefix #\?) rationalize)
  (labels
    ((tokenize-identifier (ch)
       (let ((chars (list ch)))
         (loop
           (cond
            ((eq :eof (setf ch (read-char stream nil :eof)))
             (return))
            ((ordinary-char-p ch)
             (push ch chars))
            (t
             (unread-char ch stream)
             (return))))
         (setf chars (nreverse chars))
         ;; so that variables can be distingished from nonvariables even after upcasing
         ;; if upper-case-var-prefix is a character such as #\?
         ;; tokenize adds it to the front of each identifier that starts with
         ;; either an upper-case character
         ;; or one or more of it followed by an alphabetic character
         ;; (read-infix-term "r(x,?,?1,X,?X,??X)") -> (R X ? ?1 ?X ??X ???X)
         (when (and upper-case-var-prefix
                    (or (upper-case-p (first chars))
                        (and (eql upper-case-var-prefix (first chars))
                             (dolist (c (rest chars) nil)
                               (cond
                                ((alpha-char-p c)
                                 (return t))
                                ((not (eql upper-case-var-prefix c))
                                 (return nil)))))))
           (setf chars (cons upper-case-var-prefix chars)))
         (operator-lookup
          (ecase (if (and (eql #\$ (first chars)) (rest chars) (eql #\$ (second chars)))
                     (readtable-case *readtable*)	; use Lisp reader case for $$ words so that $$sum is read as $$SUM if reader upcases
                     case)
            (:preserve (coerce chars 'string))
            (:upcase (nstring-upcase (coerce chars 'string)))
            (:downcase (nstring-downcase (coerce chars 'string)))))))
     (tokenize-special (ch)
       (let ((chars (list ch)))
         (loop
           (cond
            ((eq :eof (setf ch (read-char stream nil :eof)))
             (return))
            ((and (not (ordinary-char-p ch))
                  (not (separator-char-p ch))
                  (not (whitespace-char-p ch))
                  (not (quotation-char-p ch))
                  (not (comment-char-p ch)))
             (push ch chars))
            (t
             (unread-char ch stream)
             (return))))
         (operator-lookup (coerce (nreverse chars) 'string))))
     (tokenize-number (ch)
       (let ((num (digit-char-p ch)) (n 0) (d 1) cv float ratio (exponent nil))
         (loop
           (cond
            ((eq :eof (setf ch (read-char stream nil :eof)))
             (return))
            ((setf cv (digit-char-p ch))
             (cond
              (float
               (setf n (+ (* 10 n) cv) d (* 10 d)))
              (ratio
               (setf n (+ (* 10 n) cv)))
              (t
               (setf num (+ (* 10 num) cv)))))
            ((and (not (or float ratio)) (eql #\. ch))
             (setf float t))
            ((and (not (or float ratio)) (eql #\/ ch))
             (setf ratio t))
            ((and (not ratio) (or (eql #\E ch) (eql #\e ch)))
             (setf exponent (tokenize-exponent))
             (return))
            (t
             (unread-char ch stream)
             (return))))
         (cond
          (float
           (setf num (+ num (/ n d))))
          (ratio
           (setf num (/ num n))))
         (when exponent
           (setf num (* num (expt 10 exponent))))
         (when (and float (not rationalize))
           (setf num (float num)))
         num))
     (tokenize-exponent ()
       (let ((negative nil) (exponent 0) ch cv)
         (cond
          ((eq :eof (setf ch (read-char stream nil :eof)))
           (return-from tokenize-exponent nil))
          ((setf cv (digit-char-p ch))
           (setf exponent cv))
          ((eql #\- ch)
           (setf negative t))
          ((eql #\+ ch)
           )
          (t
           (unread-char ch stream)
           (return-from tokenize-exponent nil)))
         (loop
           (cond
            ((eq :eof (setf ch (read-char stream nil :eof)))
             (return))
            ((setf cv (digit-char-p ch))
             (setf exponent (+ (* 10 exponent) cv)))
            (t
             (unread-char ch stream)
             (return))))
         (if negative (- exponent) exponent)))
     (tokenize-string (quotechar)
       (let ((chars nil) ch)
         (loop
           (cond
            ((eql quotechar (setf ch (read-char stream t)))
             (setf chars (nreverse chars))
             (return (ecase quotechar
                       (#\"
                        (coerce chars 'string))
                       (#\'
                        ;; any characters can be put into a symbol by using '...' quotation
                        ;; this suppresses default case mangling, var-prefixing, and operator lookup
                        ;; to disambiguate tokenization of ? and '?' etc.
                        ;; '?...'  is tokenized as |^A?...| that is later replaced by ($$quote ?...)
                        (cond
                         ((and chars 
                               (or (eql upper-case-var-prefix (first chars))
                                   (eql (code-char 1) (first chars))))
                          (make-symbol (coerce (cons (code-char 1) chars) 'string)))
                         (t
                          (intern (coerce chars 'string))))))))
            ((eql #\\ ch)
             (push (read-char stream t) chars))
            (t
             (push ch chars))))))
     (operator-lookup (name)
       ;; return an operator interpretation if there is one
       ;; we can lookup the correct interpretation later
       (or (infix-operator-lookup name)
           (prefix-operator-lookup name)
           (postfix-operator-lookup name)
           (intern name))))
    (let (ch)
      (loop
        (cond
         ((eq :eof (setf ch (read-char stream nil :eof)))
          (return-from tokenize1 none))
         ((whitespace-char-p ch)
          )
         ((comment-char-p ch)
          ;; comment from comment-char through end of line
          (loop
            (when (or (eql #\newline (setf ch (read-char stream t))) (eql #\return ch) (eql #\linefeed ch))
              (return))))
         ((and (eql #\/ ch) (eql #\* (peek-char nil stream nil :eof)))
          ;; comment from /* through */
          (read-char stream)
          (loop
            (when (eql #\* (read-char stream t))
              (if (eql #\/ (setf ch (read-char stream t)))
                  (return)
                  (when (eql #\* ch)
                    (unread-char ch stream))))))
         ((separator-char-p ch)
          (return ch))
         ((digit-char-p ch)
          (return (tokenize-number ch)))
         ((ordinary-char-p ch)
          (return (tokenize-identifier ch)))
         ((quotation-char-p ch)
          (return (tokenize-string ch)))
         ((or (eql #\- ch) (eql #\+ ch))
          (return (if (digit-char-p (peek-char nil stream nil #\a))
                      (let ((v (tokenize-number (read-char stream))))
                        (if (eql #\- ch) (- v) v))
                      (tokenize-special ch))))
         (t
          (return (tokenize-special ch))))))))

(defun tokenize (stream &key (case :preserve) (upper-case-var-prefix #\?) rationalize)
  (let ((tokens nil))
    (loop
      (let ((token (tokenize1 stream :case case :upper-case-var-prefix upper-case-var-prefix :rationalize rationalize)))
        (if (eq none token)
            (return)
            (push token tokens))))
    (nreverse tokens)))

;;; converts "p(a,b,c)" to (p a b c)
;;; converts "[a,b,c]" to ($$list a b c)
;;; converts "[a,b|c]" to ($$list* a b c)

(defun tokens-to-lisp (tokens)
  (let ((stack '(#\.))					;stack contains terms, operators, #\(s, #\.
        token1)
    (labels
      ((tokens-to-lisp1 ()
         (cond
          ((or (eql #\( token1) (numberp token1) (stringp token1))
           (cond
            ((starting-term)
             (push token1 stack))
            (t
             (syntax-error 1))))
          ((symbolp token1)
           (cond
            ((starting-term)
             (push (if (eql #\( (first tokens))
                       (progn
                         (setf tokens (rest tokens))
                         (cons token1 (tokens-to-lisp2 '(#\)))))
                       token1)
                   stack))
            (t
             (syntax-error 2))))
          ((eql #\[ token1)
           (cond
            ((starting-term)
             (push (tokens-to-lisp2 '(#\])) stack))
            (t
             (syntax-error 3))))
          ((eql #\) token1)
           (cond
            ((not (starting-term))
             (reduce-all #\())
            (t
             (syntax-error 4))))
          ((operator-p token1)
           ;; is it the right kind of operator?
           ;; if not, just use it as a symbol
           (setf token1 (operator-input-string token1))
           (cond
            ((starting-term)
             (cond
              ((operator-p (setf token1 (or (prefix-operator-lookup token1) (intern token1))))
               (push token1 stack))
              (t
               (tokens-to-lisp1))))
            (t
             (cond
              ((operator-p (setf token1 (or (infix-operator-lookup token1) (postfix-operator-lookup token1) (intern token1))))
               (reduce-before token1)
               (push token1 stack))
              (t
               (tokens-to-lisp1))))))
          (t
           (syntax-error 5))))
       (tokens-to-lisp2 (brackets)
         ;; convert lists and argument lists
         (let ((list* nil)
               (args nil)
               (l nil))
           (loop
             (cond
              ((or (null tokens) (eql #\. (setf token1 (pop tokens))))
               (syntax-error 6))
              ((eql #\( token1)
               (push #\) brackets)
               (push token1 l))
              ((eql #\[ token1)
               (push #\] brackets)
               (push token1 l))
              ((or (eql #\) token1) (eql #\] token1))
               (cond
                ((not (eql token1 (pop brackets)))
                 (syntax-error 7))
                ((null brackets)
                 (cond
                  ((null l)
                   (when args
                     (syntax-error 8)))
                  (t
                   (push (tokens-to-lisp (nreverse l)) args)))
                 (setf args (nreverse args))
                 (return (if (eql #\] token1) (cons (if list* '$$list* '$$list) args) args)))
                (t
                 (push token1 l))))
              ((and (null (rest brackets))
                    (eql #\] (first brackets))
                    ;; treat vertical bar as a separator only in lists
                    (cond
                     ((symbolp token1)
                      (when (string= "|" (symbol-name token1))
                        (setf token1 #\|))
                      nil)
                     ((operator-p token1)
                      (when (string= "|" (operator-input-string token1))
                        (setf token1 #\|))
                      nil)
                     (t
                      nil)))
               )
              ((and (null (rest brackets)) (or (eql #\, token1) (and (eq #\| token1) (eql #\] (first brackets)))))
               (cond
                ((null l)
                 (syntax-error 9))
                (list*
                 (syntax-error 10))
                (t
                 (push (tokens-to-lisp (nreverse l)) args)))
               (setf l nil)
               (setf list* (eq #\| token1)))
              (t
               (push token1 l))))))
       (reduce-once ()
         (let ((x (pop stack)) (y (pop stack)) z)
           (cond
            ((infix-operator-p y)
             (if (and (operand-p (setf z (pop stack))) (operand-p x))
                 (push (list (operator-output-symbol y) z x) stack)
                 (syntax-error 11)))
            ((prefix-operator-p y)
             (if (operand-p x)
                 (push (list (operator-output-symbol y) x) stack)
                 (syntax-error 12)))
            ((postfix-operator-p x)
             (if (operand-p y)
                 (push (list (operator-output-symbol x) y) stack)
                 (syntax-error 13)))
            (t
             (syntax-error 14)))))
       (reduce-before (op)
         (loop
           (if (cond
                ((operator-p (first stack))
                 (reduce-before? (first stack) op))
                ((operator-p (second stack))
                 (reduce-before? (second stack) op))
                (t
                 nil))
               (reduce-once)
               (return))))
       (reduce-all (start)
         (loop
           (cond
            ((and (operand-p (first stack)) (eql start (second stack)))
             (setf stack (cons (first stack) (rrest stack)))
             (return))
            (t
             (reduce-once)))))
       (starting-term ()
         (let ((top (first stack)))
           (not (or (operand-p top) (postfix-operator-p top)))))
       (operand-p (x)
         (not (or (eql #\( x) (eql #\. x) (operator-p x))))
       (syntax-error (name)
         (error "Syntax error ~A at or before~{ ~S~}~% token1 = ~S~% stack =~{ ~S~}" name (firstn tokens 20) token1 stack)))
      (loop
        (cond
         ((or (null tokens) (eql #\. (setf token1 (pop tokens))))
          (reduce-all #\.)
          (return))
         (t
          (tokens-to-lisp1))))
      (values (if (null (rest stack)) (first stack) stack) tokens))))

(defun read-infix-term (x &key (case :preserve) (upper-case-var-prefix #\?) rationalize)
  ;; read one term from x and return it and list of leftover tokens
  ;; if x is a string, tokenize it
  ;; if x is a list, assume it is a tokenized string (with correct case and upper-case-var-prefix)
  (when (stringp x)
    (with-input-from-string (stream x)
      (setf x (tokenize stream :case case :upper-case-var-prefix upper-case-var-prefix :rationalize rationalize))))
  (cl:assert (consp x))
  (tokens-to-lisp x))

(defun read-infix-terms (x &key (case :preserve) (upper-case-var-prefix #\?) rationalize)
  (when (string x)
    (with-input-from-string (stream x)
      (setf x (tokenize stream :case case :upper-case-var-prefix upper-case-var-prefix :rationalize rationalize))))
  (let ((terms nil) terms-last term)
    (loop
      (cond
       ((null x)
        (return terms))
       (t
        (setf (values term x) (tokens-to-lisp x))
        (collect term terms))))))

;;; infix-reader.lisp EOF
