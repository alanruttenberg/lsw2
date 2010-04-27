(defvar *handle-resolver* nil)

(defparameter *handle-value-decoders*
  (list
   (list "HS_ADMIN" 'AdminRecord #"decodeAdminRecord" 'hdllib.Encoder #"toString")
   (list ".*HS_SITE" 'SiteInfo #"decodeSiteInfoRecord" 'hdllib.Encoder #"toString")
   (list "HS_VLIST" nil #"decodeValueReferenceList" 'hdllib.Encoder
	 (lambda(result)
	   (map 'list (lambda(el) (#"toString" el)) result)))
   (list "HS_PUBKEY" nil #"getPublicKeyFromBytes" 'hdllib.Util 'identity)
   ))

(defun decode-handle-value (handle-value)
  (let* ((type (#"getTypeAsString" handle-value))
	 (value 
	  (loop for (pattern rec decoder1 class decoder2 ) in *handle-value-decoders*
	       when (#"matches" type pattern )
	       do (let* ((record (and rec (new rec)))
			(result
			 (apply decoder1 class (#"getData" handle-value) 0
				(and record (list record)))))
		    (return
		      (funcall decoder2 (or record result))))
	       finally 
	       (return (#"getDataAsString" handle-value)))))
    (list type value)))
  
(defun resolve-handle (handle &optional resolver)
  (unless resolver
    (unless *handle-resolver*
      (setq *handle-resolver* (new 'handleresolver)))
    (setq resolver *handle-resolver*))
  (let* ((encoded (#"encodeString" 'net.handle.hdllib.Util handle))
	 (null (make-immediate-object nil :ref))
	 (resp (#"processRequest" resolver (new 'ResolutionRequest encoded null null null))))
    (if (equal (#"toString" (jobject-class resp))  "class net.handle.hdllib.ErrorResponse")
	(error (#"toString" resp))
	(map 'list 'decode-handle-value (#"getHandleValues" resp)))))


		    
