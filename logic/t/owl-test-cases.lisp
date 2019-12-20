(in-package :logic)

;; Scrape a testcase off the wiki.
;; Some tests have a single ontology (e.g. consistency) others 2 (e.g. entailement)
;; Returns a list of lists. Each list has
;; - The raw scraped text
;; - The kb object - loaded from a temp file the raw was written to
;; - A list of FOL formulas translated from the OWL

(defun get-owl-test-case (url)
  (let* ((it (cl-user::get-url url))
         ;; Which OWL profiles is this valid for. We're not interested in tests that are solely OWL Full.
         (profiles (mapcar 'car (all-matches (caar (all-matches it "(?s)<th>\\s*Syntactic Species/Profile.*?([(]|(</tr>))" 0))
                                             "title=\"([^\"]*)" 1)))
         (onts (mapcar 'car (all-matches it "(?si)<div class=\"smwpre\">(.*?)</div>" 1)))
         ;; ugly - scraping is never pretty
         (clean
           (mapcar (lambda(ont)
                      (#"replaceAll" 
                       (#"replaceAll" 
                        (#"replaceAll" 
                         (#"replaceAll" 
                          (jss::replace-all ont
                                            "&#(x?)([A-Fa-f0-9][A-Fa-f0-9]);"
                                            (lambda(x e) (let ((*read-base* (if (equal x "") 10 16)))
                                                           (string (code-char (read-from-string e)))))
                                            1 2) "(?i)&(amp;)?lt;" "<") "(?i)&(amp;)?gt;" ">") "(?i)&(amp;)?nbsp;" " ") "(?i)<br\\s*/>" " "))
                   onts)))
    ;; We don't do datatype tests now. Mark them by adding "Data" to the profiles
    (if (or
         (some (lambda(ont) (search "DatatypeProperty" ont)) clean)
         (some (lambda(ont) (search "DataProperty" ont)) clean))
        (push "Data" profiles))
    ;; We don't handle tests that import. Mark them by adding "Imports" to the profiles
    (if (some (lambda(ont) (search "owl:imports" ont)) clean)
        (push "Imports" profiles))
    
    (list profiles
          (loop for ont in clean
                collect
                ;; write the OWL to a file
                (let ((tmp (uiop/stream::get-temporary-file :suffix ".owl")))
                  (with-open-file (f tmp :direction :output)
                    (write-string ont f))
                  ;; load ontology from the file
                  (let ((kb (cl-user::load-ontology (namestring tmp))))
                    (let (axs)
                      ;; translate to FOL
                      (cl-user::each-axiom kb (lambda(ax) 
                                                (let ((l (cl-user::axiom-to-lisp-syntax ax)))
                                                  (push (replace-with-labels-or-uri-tail-symbol l kb ) axs)))) 
                      (list ont kb axs))))))))


;; There are four kinds of test cases.
;; Consistency and Inconsistency tests, which take a single ontology
;; Positive and negative entailment tests, which take two ontologies

;; So far the only set used. Last count there were 38
(defun check-positive-entailment-test-case  (url &optional debug (reasoner 'z3-prove))
  (when (not (eql 0 (search "http" url)))
      (setq url (format nil "http://owl.semanticweb.org/page/~a.html" url)))
  (destructuring-bind (profiles ((raw1 o1 o1axs) (raw2 o2 o2axs))) (get-owl-test-case url)
    (when debug
      (setf (symbol-value 'o1) o1)
      (setf (symbol-value 'o2) o2))
    (if (member "Data" profiles :test 'equalp)
        (progn (warn "skipping ~a - uses data" url)
               (return-from check-positive-entailment-test-case t))
        (if (member "Imports" profiles :test 'equalp)
            (progn (warn "skipping ~a - imports other ontologies" url)
                   (return-from check-positive-entailment-test-case t))
            (if (not (intersection profiles '("Test:DL" "Test:EL" "Test:QL" "Test:RL") :test 'equalp))
                (progn
                  (warn "skipping ~a - not OWL direct semantics" url)
                  (return-from check-positive-entailment-test-case t))
                ;; We need to rename variables since our standard set commonly used in the ontologies
                (let ((ant `((:and ,@(rename-variables (mapcar 'owl-sexp-to-fol (remove-if 'axiom-has-no-logical-consequence  o1axs))))))
                      (cons (when o2axs `(:and ,@(rename-variables (mapcar 'owl-sexp-to-fol (remove-if 'axiom-has-no-logical-consequence  o2axs)))))))
                  (when (or (null cons) (equal cons '(:and)))
                    (warn "Empty consequent. Skipping")
                    (return-from check-positive-entailment-test-case o2 ))
                  (when (and debug (equalp ant '((:and))))
                    (warn "Empty antecedent, checking consequent is tautology"))
                  (if debug
                      (progn (print-db profiles raw1 o1 ant raw2 o2 cons)
                             )
                      (if (equalp ant '((:and)))
                          (eq (z3-check-true (fix-conflicting-symbols cons)) :proved)
                          (eq (funcall reasoner `(:rdf-type-separation
                                                  ,@(fix-conflicting-symbols ant)) 
                                       (fix-conflicting-symbols cons)) :proved))
                      )))))))

(defun fix-conflicting-symbols (form)
  (jss::tree-replace (lambda(e)
                       (or (second (assoc e '((fp fp_x))))
                           e))
                     form))

;; Needs more logic - WIP
(defun check-satisfiability-test-case  (url)
  (let ((axs  (car (get-owl-test-case url))))
    (eq (z3-check-satisfiability `((:and ,@(mapcar 'owl-sexp-to-fol axs)))) :sat)))

;; Helper. Renames all variable to ones with the suffix _x
(defun rename-variables (form)
  (tree-replace (lambda(form)
                  (if (and (symbolp form)
                           (char= (char (string form) 0) #\?))
                      (intern (concatenate 'string (string form) "_X"))
                      form))
                form))
                        
;; Helper function while debugging
;; *all-owl-tests*, *skip-owl-tests* are in t/owl-to-fol.lisp
;; *all-owl-tests* is a list of names of tests, almost verbatim from the wiki
;; *skip-owl-tests* is a list of pairs a names and keywords explaining why we won't use this test.
;; Current reasons :not-dl, :empty-consequent, :data, :imports
(defun run-owl-tests (&optional (tests *all-owl-tests*))
  (loop with count = 0
        for test in tests
        for url = (format nil "http://owl.semanticweb.org/page/~a.html" test)
        unless (or (consp test) (find test *skip-owl-tests* :key 'car :test 'equalp))
          do (print test)
             (incf count)
             (print-db (check-positive-entailment-test-case url))
        finally (return count)))
