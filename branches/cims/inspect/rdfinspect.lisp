(in-package :cl-user)

;;; Specialize inspector for RDF browsing.

(defclass rdf-inspector (inspector)
  ((object :initform nil)
   (reasoner :initarg :reasoner 
	     :initform *default-reasoner*)
   ;; more general to default this to nil
   (from-graphs :initarg :from-graphs
		:initform nil  ;;; (list !<http://purl.org/commons/hcls/20070416>)
		:accessor from-graphs)
   (show-inverse :initarg :show-inverse
		 :initform nil)
   (limit :initform 100 :initarg :limit)
   ))

;;; doesn't work
(defmethod make-contents :around ((this rdf-inspector))
  (with-slots (show-inverse) this
    (let ((panel (call-next-method)))
      (when show-inverse
	(#"setSize" panel 600 300))
      panel)))


(defmethod oinspect-data ((inspector rdf-inspector) (uri null))
  (with-slots (reasoner object show-inverse limit) inspector
    (cons '("Subject" "Predicate" "Object")
	(sparql `(:select (?subject ?prop ?object) 
		  (:limit ,limit :distinct t :from ,(from-graphs inspector))
		  (?subject ?prop ?object))
		:use-reasoner reasoner))))

(defmethod oinspect-data ((inspector rdf-inspector) (uri uri))
  (with-slots (reasoner object show-inverse limit) inspector
    (let ((forwards
	   (sparql `(:select (?prop ?value) 
			     (:limit ,limit :distinct t :from ,(from-graphs inspector)) ;+++ artificial, but prevents runaway
			     (,uri ?prop ?value))
		   :use-reasoner reasoner))
	  (backwards (and show-inverse
			  (sparql `(:select (?subject ?prop) 
				    (:limit ,limit :distinct t :from ,(from-graphs inspector)) ;+++ artificial, but prevents runaway
				    (?subject ?prop ,uri))
				  :use-reasoner reasoner))))
      (if show-inverse
	  (cons '("Subject" "Predicate" "Object")
		(nconc (mapcar #'(lambda (row)
				   (cons object row))
			       forwards)
		       (mapcar #'(lambda (row)
				   (nconc row (list object)))
			       backwards)))
	(cons '("Predicate" "Object")
	      forwards
	      )))))


(defmethod ojump ((inspector rdf-inspector) push fieldvalue)
  ;; show row for turning into query
  (multiple-value-bind (field row)
      (selected-field inspector)
    (print row))
  (call-next-method))
#|
   ;; Pass on info to new inspector
  (with-slots (reasoner from-graphs show-inverse) inspector
    (make-instance 'rdf-inspector 
      :object fieldvalue
      :reasoner reasoner
      :show-inverse show-inverse
      :from-graphs from-graphs))) |#

(defclass rdf-inspector-window (rdf-inspector jframe) 
  ())

;;; convenience.  also works for uri = :jena
(defun oinspect-sparql-endpoint (uri &optional from-graphs)
  (make-instance 'rdf-inspector-window
		 :reasoner (if (stringp uri) 
			       (make-uri uri)
			       uri)
		 :show-inverse t
		 :from-graphs (mapcar #'make-uri from-graphs)
		 :object nil))
