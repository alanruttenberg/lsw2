(defun get-sheets (&key file only-sheets)
  (let* ((workbook (#"create" 'workbookfactory (new 'fileinputstream (namestring (truename file)))))
	 (sheets (loop for n below (#"getNumberOfSheets" workbook)
		    collect (list (#"getSheetName" workbook n) (#"getSheetAt" workbook n))))
	 (contents (loop for sheet in sheets
		      when (or (not only-sheets)
			       (member (car sheet)  only-sheets :test 'equal))
		      append (get-sheet (second sheet) (first sheet)))))
    contents
    ))

(defun list-sheets (&key file)
  (let* ((workbook (#"create" 'workbookfactory (new 'fileinputstream (namestring (truename file))))))
    (loop for n below (#"getNumberOfSheets" workbook)
		    collect (list (#"getSheetName" workbook n) (#"getSheetAt" workbook n)))
    ))

(defun get-cell-value (cell)
  (and cell
       (let ((type (#"getCellType" cell))
	     (formula nil))
	 (when (eql type (load-time-value (get-java-field 'org.apache.poi.hssf.usermodel.HSSFCell "CELL_TYPE_FORMULA")))
	   (setq formula (#"getCellFormula" cell))
	   (setq type (#"getCachedFormulaResultType" cell)))
	 (let ((value
		(cond ((eql type (load-time-value (get-java-field 'org.apache.poi.hssf.usermodel.HSSFCell "CELL_TYPE_STRING")))
		       (#"getStringCellValue" cell))
		      ((eql type (load-time-value (get-java-field 'org.apache.poi.hssf.usermodel.HSSFCell "CELL_TYPE_NUMERIC")))
		       (#"getNumericCellValue" cell))
		      ((eql type (load-time-value (get-java-field 'org.apache.poi.hssf.usermodel.HSSFCell "CELL_TYPE_BLANK")))
		       nil)
		      ((eql type (load-time-value (get-java-field 'org.apache.poi.hssf.usermodel.HSSFCell "CELL_TYPE_BOOLEAN")))
		       (#"getBooleanCellValue" cell))
		      ((eql type (load-time-value (get-java-field 'org.apache.poi.hssf.usermodel.HSSFCell "CELL_TYPE_ERROR")))
		       `(:error ,(#"getErrorCellValue" cell)))
		      (t (error "unknown cell type ~a ~a" cell type)))))
	   (values value formula (#"getCellStyle" cell))))))


;; 	   (if formula
;; 	       (values value formula)
;; 	       value)))))

(defun get-sheet (sheet sheet-name)
  (destructuring-bind (headers . rows)
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
						   (and cell (get-cell-value cell))))))
	 collect potential)
    (let ((headerkeys (mapcar (lambda(s)
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
  (loop with first = (#"getFirstRowNum" sheet)
     for rowno from first to (#"getLastRowNum" sheet) 
     with nocells = (loop for row below (#"getPhysicalNumberOfRows" sheet)
		       maximize (or (and (not (#"getRow" sheet row)) 0)
				    (#"getPhysicalNumberOfCells" (#"getRow" sheet row))))
     for row = (#"getRow" sheet rowno)
     for cell-list = (and row 
			  (loop for colno below nocells
			     for colcount from 1
			     for cell = (#"getCell" row colno)
			     collect (get-cell-value cell)))
     ;do (print-db row cell-list (#"getRowNum" row)) (when (equal (car cell-list) "nlsb") (push row @@))
     collect cell-list))

