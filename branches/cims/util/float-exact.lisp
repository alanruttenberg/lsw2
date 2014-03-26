;; compare http://www.h-schmidt.net/FloatApplet/IEEE754.html

(defun float-exact (float digits) 
  "Take a float and a number of digits and convert to the exact real"
  (let* ((bits  (#"floatToRawIntBits" 'Float (new 'float float)))
	 (mode (get-java-field 'java.math.BigDecimal "ROUND_UNNECESSARY"))
	 (two (new 'java.math.bigdecimal 2))
	 (addend (#"divide" one two digits mode))
	 (sum (#"setScale" (new 'java.math.bigdecimal 1) digits))
	 (half (#"divide" one two 1 mode))
	 (exponent (- (ldb (byte 8 23) bits) 127))
	 (mult (if (>= exponent 0)  two half)))
    (loop for bit from 22 downto 0
       do 
       (if (logbitp bit bits)
	   (setq sum (#"add" sum addend)))
       (setq addend (#"divide" addend two digits mode))
       finally (return sum))
    (loop  repeat (abs exponent)
       do (setq sum (#"multiply" sum mult )))
    (if (logbitp 31 bits) (setq sum (#"subtract" (new 'java.math.BigDecimal 0) sum)))
    (values sum
	    (#"toString" sum)
	    (format nil "~23,'0b" (ldb (byte 23 0) bits))
	    (format nil "~7,'0b" (ldb (byte 7 24) bits))
	    )))

;; (float-exact "0.1")
;; =>
;; "0.10000000149011611938476562500"
;; "10011001100110011001101"
;; "0111101"

;; (float-exact "2.1" 25)
;; =>
;; #<JAVA-OBJECT java.math.BigDecimal {44CB1D}>
;; "2.0999999046325683593750000"
;; "00001100110011001100110"
;; "1000000"
