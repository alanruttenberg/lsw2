
;; skip-rows - how many initial rows in the spreadsheet until the header line. default:0
;; row-limit - how many rows to read. Includes skipped rows and header row. default nil = no limit
;; with-style? - beginning of parsing style. For now only cell background color. default nil
;;    With this argument t, when there is a non-default background color
;;    the cell value will be returned as
;;    (:with-style <value> (:cell-background-color (<r> <g> <b>)))
;;    r,g,b are 0-255  
;;    default: nil
;; get-hyperlink? - if the cell is a hyperlink, return value as (:link <url> <label>), 
;;    where the url and label are both strings. Without this argument only the label is returned.
;;    default nil

(defun get-sheets (&key file only-sheets (skip-rows 0) row-limit with-style? get-hyperlink?)
  (let* ((workbook (#"create" 'workbookfactory (new 'fileinputstream (namestring (truename file)))))
	 (sheets (loop for n below (#"getNumberOfSheets" workbook)
		    collect (list (#"getSheetName" workbook n) (#"getSheetAt" workbook n))))
	 (contents (loop for sheet in sheets
		      when (or (not only-sheets)
			       (member (car sheet)  only-sheets :test 'equal))
		      append (get-sheet (second sheet) (first sheet) :skip-rows skip-rows :row-limit row-limit
					:with-style? with-style? :process-hyperlink?  get-hyperlink?
					))))
    contents
    ))

(defun list-sheets (&key file)
  (let* ((workbook (#"create" 'workbookfactory (new 'fileinputstream (namestring (truename file))))))
    (loop for n below (#"getNumberOfSheets" workbook)
		    collect (list (#"getSheetName" workbook n) (#"getSheetAt" workbook n)))
    ))


;; 	   (if formula
;; 	       (values value formula)
;; 	       value)))))


;; It is a pita to get style information. There are different models for colors, etc.
;; http://poi.apache.org/apidocs/org/apache/poi/xssf/usermodel/XSSFColor.html#getIndexed()
:; I'll add a bit at a time, starting with cell background color

(defun get-cell-styled-value (value style)
  (let ((background (let ((color (#"getFillForegroundXSSFColor" style)))
		      (and color
			   (let ((rgb (#"getRgb" color)))
			     (and rgb
				  (list
				   ;;(mod (+ 256 (jarray-ref rgb 0)) 256) ;; should return just rgb but returns alpha first
				   (mod (+ 256 (jarray-ref rgb 1)) 256)
				   (mod (+ 256 (jarray-ref rgb 2)) 256)
				   (mod (+ 256 (jarray-ref rgb 3)) 256))))))))
    (if background 
	`(:with-style ,value (:cell-background-color ,background))
	value)
    ))

(defun parse-hyperlink-formula (formula)
  `(:link ,@(car (all-matches formula "HYPERLINK\\(\"([^\"]+)\",\"([^\"]+)\"\\)" 1 2))))

(defun get-sheet (sheet sheet-name &key (skip-rows 0) row-limit process-hyperlink? with-style?)
  (let ((rows
	 (loop with first = (#"getFirstRowNum" sheet)
	    for rowno from first to (#"getLastRowNum" sheet)
	    with nocells = (loop for row below (#"getPhysicalNumberOfRows" sheet)
			      maximize (or (and (not (#"getRow" sheet row)) 0)
					   (#"getPhysicalNumberOfCells" (#"getRow" sheet row))))
	    for row = (#"getRow" sheet rowno)
	    for potential = (and row 
				 (list sheet-name rowno
				       (loop for colno below nocells
					  for colcount from 1
					  for cell = (#"getCell" row colno)
					  collect (if (and (equal rowno first) 
							   (or (null cell)
							       (and (not (null cell))
								    (or (null (get-cell-value cell))
									(equal (get-cell-value cell) "")))))
						      (format nil "Column-~a" colcount)
						      (if cell
							  (multiple-value-bind (value formula style)
							      (get-cell-value cell)
							    (when (and process-hyperlink? formula (#"matches" formula "^HYPERLINK.*"))
							      (setq value (parse-hyperlink-formula formula)))
							    (if with-style?
								(get-cell-styled-value value style)
								value))))
							      )))
	    while (or (null row-limit) (not (minusp (decf row-limit))))
	    collect potential)))
    (loop repeat skip-rows do (setq rows (cdr rows)))
    (let* ((headers (car rows))
	   (rows (cdr rows))
	   (headerkeys (mapcar (lambda(s)
				 (if (consp s) (setq s (second s)))
				 (intern (substitute #\- #\space (string-upcase s))'keyword)) (third headers))))
      (loop for (sheet rowno row) in rows
	 when rowno
	 collect
	   (append `((:sheet ,sheet) (:row ,(1+ rowno)))
		   (loop for key in headerkeys
		      for cell in row
		      if (and (equal key :synonym) cell)
		      do (if (equal cell "")
			     (setq cell nil)
			     (setq cell (mapcar (lambda(e) (string-trim " " e)) (split-at-regex cell "[;,]"))))
		      collect (list key cell)))))))

(defun get-sheet-as-row-lists (sheet)
  (let ((them nil))
    (each-row-in-sheet nil (lambda(row) (push row them)) sheet)
    (nreverse them)))

;; before optimization 
;; 76.037 seconds real time
;; 51375663 cons cells

;; after optimization
;; 31.865 seconds real time
;; 1701517 cons cells

(defun each-row-in-sheet  (file fn &key (sheet :first))
  (declare (optimize (speed 3) (safety 0)))
  (let* ((sheets (unless (java-object-p sheet) (list-sheets  :file file)))
	 (selected-sheet (if (java-object-p sheet) 
			     sheet
			     (if (eq sheet :first)
				 (second (first sheets))
				 (second (assoc (string sheet) sheets :test 'equalp))))))
    (assert selected-sheet () "couldn't find sheet ~a" selected-sheet)
    (jss:with-constant-signature ((getrow "getRow") (getcell "getCell") (get-number-of-cells "getPhysicalNumberOfCells"))
      (loop with first = (#"getFirstRowNum" selected-sheet)
	 for rowno from first to (#"getLastRowNum" selected-sheet) 
	 with nocells = (loop for rownum below (#"getPhysicalNumberOfRows" selected-sheet)
			   for row = (getrow selected-sheet rownum)
			   maximize (or (and (not row) 0)
					(get-number-of-cells row)))
	 for row = (getrow selected-sheet rowno)
	 for cell-list = (and row 
			      (loop for colno below nocells
				 for colcount from 1
				 for cell = (getcell row colno)
				 collect (get-cell-value cell)))
	 do (funcall fn cell-list)))))
     

(defun get-cell-value (cell)
  (declare (optimize (speed 3) (safety 0)))
  (with-constant-signature ((getcelltype "getCellType")
			    (geterror "getErrorCellValue")
			    (getstring "getStringCellValue")
			    (getnumber "getNumericCellValue")
			    (getboolean "getBooleanCellValue")
			    (getstyle "getCellStyle")
			    (getformula "getCellFormula")
			    (getformulatype "getCachedFormulaResultType"))
    (and cell
	 (let ((type (getcelltype cell))
	       (formula nil))
	   (when (eql type (load-time-value (get-java-field 'org.apache.poi.hssf.usermodel.HSSFCell "CELL_TYPE_FORMULA")))
	     (setq formula (getformula cell))
	     (setq type (getformulatype cell)))
	   (let ((value
		  (cond ((eql type (load-time-value (get-java-field 'org.apache.poi.hssf.usermodel.HSSFCell "CELL_TYPE_STRING")))
			 (getstring cell))
			((eql type (load-time-value (get-java-field 'org.apache.poi.hssf.usermodel.HSSFCell "CELL_TYPE_NUMERIC")))
			 (getnumber cell))
			((eql type (load-time-value (get-java-field 'org.apache.poi.hssf.usermodel.HSSFCell "CELL_TYPE_BLANK")))
			 nil)
			((eql type (load-time-value (get-java-field 'org.apache.poi.hssf.usermodel.HSSFCell "CELL_TYPE_BOOLEAN")))
			 (getboolean cell))
			((eql type (load-time-value (get-java-field 'org.apache.poi.hssf.usermodel.HSSFCell "CELL_TYPE_ERROR")))
			 `(:error ,(geterror cell)))
			(t (error "unknown cell type ~a ~a" cell type)))))
	     (values value formula (getstyle cell)))))))