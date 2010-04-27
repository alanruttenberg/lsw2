(defclass parsed-book ()
  ((book-path :accessor book-path :initarg :book-path :initform nil)
   (parsed-sheets :accessor parsed-sheets :initarg :parsed-sheets :initform nil)
   (parsed-blocks :accessor parsed-blocks :initarg :parsed-blocks :initform nil)
   (sheet-type :accessor sheet-type :allocation :class :initform 'parsed-sheet)))

(defmethod after-all-sheets-parsed ((b parsed-book)))

(defmethod print-object ((o parsed-book) stream)
  (let ((*print-case* :downcase))
    (print-unreadable-object (o stream :type t :identity nil)
      (format stream "~s - ~a sheets, ~a blocks"
	      (book-path o)
	      (length (parsed-sheets o))
	      (length (parsed-blocks o))))))

(defmethod report-parse-results ((b parsed-book))
  (format t "Parse results for book ~a~%" b) 
  (loop for sheet in (parsed-sheets b) do (report-parse-results sheet)))

(defclass parsed-sheet ()
  ((sheet-book :accessor sheet-book :initarg :sheet-book :initform nil)
   (sheet-name :accessor sheet-name :initarg :sheet-name :initform nil) 
   (parsed-blocks :accessor parsed-blocks :initarg :parsed-blocks :initform nil)
   (sheet-rows :accessor sheet-rows :initarg :sheet-rows :initform nil)
   (java-sheet :accessor java-sheet :initarg :java-sheet :initform nil)
   (parse-errors :accessor parse-errors :initform nil :initarg :parse-errors)))

(defmethod after-all-sheets-parsed ((b parsed-sheet)))

(defmethod report-parse-results ((s parsed-sheet))
  (format t "Parse results for sheet ~a~%" s) 
  (loop for error in (parse-errors s) do 
       (write-string error))
  (loop for block in (parsed-blocks s) do (report-parse-results block)))

(defmethod print-object ((o parsed-sheet) stream)
  (let ((*print-case* :downcase))
    (print-unreadable-object (o stream :type t :identity nil)
      (format stream "~a in ~a - ~a blocks"
	      (sheet-name o)
	      (book-path (sheet-book o))
	      (length (parsed-blocks o))))))

(defclass parsed-block ()
  ((in-sheet :accessor in-sheet :initarg :in-sheet :initform nil)
   (first-row :accessor first-row :initarg :first-row :initform nil)
   (block-rows :accessor block-rows :initarg :block-rows :initform nil)
   (start-column :accessor start-column :initarg :start-column :initform nil) 
   (end-column :accessor end-column :initarg :end-column :initform nil)
   (block-headers :accessor block-headers :allocation :class )
   (row-class :accessor row-class :allocation :class :initform 'parsed-cells)
   (parsed-rows :accessor parsed-rows :initarg :parsed-rows :initform nil)
   (parse-errors :accessor parse-errors :initform nil :initarg :parse-errors)))

(defmethod after-all-sheets-parsed ((b parsed-block)))

(defmethod print-object ((object parsed-block)  stream)
  (let ((*print-case* :downcase))
    (print-unreadable-object (object stream :type t :identity nil)
      (format stream "~a:[~a-~a]@~a ~a rows"
	      (sheet-name (in-sheet object))
	      (start-column object)
	      (end-column object)
	      (first-row object)
	      (length (block-rows object))))))

(defmethod report-parse-results ((b parsed-block))
  (format t "Parse results for block ~a~%" b)
  (loop for error in (parse-errors b) do 
       (write-string error))
  (loop for row in (parsed-rows b) 
       when (remove "no parsing method defined" (parse-errors row) :test 'equal) do (format t "~a:~%~{  ~a~%~}" row (parse-errors row))))

(defmethod parse-block ((block parsed-block))
  (setf (parsed-rows block)
	(loop for row in (block-rows block)
	   for count from (+ (first-row block) 1)
	   with row-class = (row-class block)
	   for row-instance  = (make-instance row-class :in-row count :in-block block :cell-list row)
	   collect row-instance
	   do (parse-row row-instance))))

(defclass parsed-cells ()
  ((in-block :accessor in-block :initarg :in-block :initform nil) 
   (in-row :accessor in-row :initarg :in-row :initform nil)
   (parse-errors :accessor parse-errors :initarg :parse-errors :initform nil)
   (cell-list :accessor cell-list :initform nil :initarg :cell-list )
   ))

(defmethod print-object ((object parsed-cells)  stream)
  (let ((*print-case* :downcase))
    (print-unreadable-object (object stream :type t :identity nil)
      (format stream "~a:[~a-~a]@~a ~a"
	      (sheet-name (in-sheet (in-block object))) (start-column (in-block object))
	      (end-column (in-block object)) (in-row object)
	      (print-summary object)))))

(defmethod print-summary ((o parsed-cells)) "")

(defmethod parse-row ((o parsed-cells))
  (setf (parse-errors o) (list "no parsing method defined"))
  )

(defun equalp-ignore-edge-space (a b)
  (if (and (stringp a) (stringp b))
      (equalp (string-trim " " a) (string-trim " " b))
      (equalp a b)))

(defun find-block (book block-type sheet)
  (loop for row in (sheet-rows sheet)
       for block-types = (block-types book)
     for rowcount from 1 with found-row
     with collecting
     with headers = (block-headers block-type)
     for found = (or found (search headers row :test 'equalp-ignore-edge-space))
					;     do (print-db found collecting row)
     when (and (eq collecting :found) (not (null (nth found row)))) do (setq collecting :collect)
     when (and found (null collecting))
     do (progn (setq collecting :found)
	       (setq found-row rowcount))
     until (and (eq collecting :collect)
		(or (null (nth found row))
		    (some (lambda(h) (search (cdr h) row :test 'equalp-ignore-edge-space)) block-types))
		)
     when (eq collecting :collect) collect
     (mapcar (lambda(e)
	       (if (stringp e) (string-trim " " e) e))
	     (subseq row found (+ found (length headers))))
     into rows
     finally
     (return-from find-block
       (and found (make-instance
		   block-type
		   :in-sheet sheet :first-row found-row :start-column found :end-column (1- (+ found (length headers)))
		   :block-rows rows  )))))

(defun locate-blocks-in-sheets (book types &optional within-sheets)
  (setf (parsed-blocks book)
	(loop
	   for (sheet-name sheet) in (list-sheets :file (book-path book))
	   for parsed-sheet =
	   (or (find sheet-name (parsed-sheets book) :test 'equalp :key 'sheet-name)
	       (let ((new 
		      (make-instance (sheet-type book) :sheet-rows (get-sheet-as-row-lists sheet) :sheet-book book :sheet-name sheet-name
				     :java-sheet sheet)))
		 (push new (parsed-sheets book))
		 new))
	   for found = (and (or (null within-sheets) (member sheet-name within-sheets :test 'equalp))
			    (loop for type in types
			       for found = (find-block book type parsed-sheet)
			       when found collect found))
	   do (setf (parsed-blocks parsed-sheet) found)
	   append found 
	   )))

