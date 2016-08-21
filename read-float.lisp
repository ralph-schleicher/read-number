;;; read-float.lisp --- read a floating-point number.

;; Copyright (C) 2016 Ralph Schleicher

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;    * Redistributions of source code must retain the above copyright
;;      notice, this list of conditions and the following disclaimer.
;;
;;    * Redistributions in binary form must reproduce the above copyright
;;      notice, this list of conditions and the following disclaimer in
;;      the documentation and/or other materials provided with the
;;      distribution.
;;
;;    * Neither the name of the copyright holder nor the names of its
;;      contributors may be used to endorse or promote products derived
;;      from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
;; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
;; ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;; Code:

(in-package :read-number)

(export 'read-float)
(defun read-float (&optional
		     (input-stream *standard-input*)
		     (eof-error-p t)
		     eof-value
		     recursive-p
		   &key
		     unsigned-number
		     (plus-sign *default-plus-sign*)
		     (minus-sign *default-minus-sign*)
		     (decimal-point *default-decimal-point*)
		     (exponent-marker *default-exponent-marker*)
		     (float-format *read-default-float-format*))
  "Read a floating-point number from an input stream.

Optional first argument INPUT-STREAM is an input stream designator.
 The default is standard input.
Optional second argument EOF-ERROR-P is a generalized boolean.
 If an end of file error occurs and EOF-ERROR-P is false, EOF-VALUE
 is returned.  Otherwise, signal an ‘end-of-file’ error.  The default
 is true.
Optional third argument EOF-VALUE is an object.  See above for more
 details.  The default is nil.
Optional fourth argument RECURSIVE-P is a generalized boolean.  True
 means that this call is expected to be embedded in a higher-level
 call to ‘read’ or a similar function used by the Lisp reader.  The
 default is false.
Keyword argument UNSIGNED-NUMBER is a generalized boolean.  True means
 to read an unsigned positive number.  If UNSIGNED-NUMBER is ‘:plus’,
 an explicit plus sign character signals a ‘parse-error’ but negative
 numbers are permitted.  The default is false.
Keyword argument PLUS-SIGN is a sequence of valid plus sign
 characters.  The plus sign is used to denote a positive number.
 The default is ‘+’.
Keyword argument MINUS-SIGN is a sequence of valid minus sign
 characters.  The minus sign is used to denote a negative number.
 The default is ‘-’.
Keyword argument DECIMAL-POINT is a sequence of valid decimal point
 characters.  The decimal point is used to separate the integer part
 of a decimal number from its fractional part.  The default is ‘.’.
Keyword argument EXPONENT-MARKER is a sequence of valid exponent
 marker characters.  The exponent marker is used to separate the
 mantissa from the exponent.  It does not specify the data type of
 the return value.  The default is ‘E’, ‘e’, ‘D’, and ‘d’.
Keyword argument FLOAT-FORMAT specifies the data type of the return
 value.  The default is ‘*read-default-float-format*’.

Return value is a floating-point number of type FLOAT-FORMAT iff a
decimal point or exponent part is present.  Otherwise, value is an
integer.

The ‘read-float’ function expects an optional sign followed by a
non-empty sequence of decimal digits.  It does recognize a decimal
point and an exponent part.  Leading or trailing whitespace is not
ignored.

If the file ends in the middle of a floating-point number
representation, ‘read-float’ signals an ‘end-of-file’ error
regardsless of the value of the EOF-ERROR-P argument.

Converting a floating-point number to the specified FLOAT-FORMAT
type may signal an ‘arithmetic-error’ condition, for example, a
‘floating-point-overflow’ or ‘floating-point-underflow’ error.

The result if undefined if the sequences of valid plus and minus
sign characters intersect."
  (check-type float-format (member short-float single-float double-float long-float))
  (with-input-from (input-stream eof-error-p eof-value recursive-p)
      (;; Bindings.
       ((sign #\+)
	(int 0)
	(frac 0)
	(exp 0)
	float)
       ;; Result.
       (let ((val (if float (coerce (* (+ int frac) (expt 10 exp)) float-format) int)))
	 (if (char= sign #\-) (- val) val)))
    ;; Number sign.
    (cond ((eq unsigned-number :plus)
	   (cond ((find next-char minus-sign :test #'char=)
		  (setf sign #\-)
		  (next-char))))
	  ((not unsigned-number)
	   (cond ((find next-char minus-sign :test #'char=)
		  (setf sign #\-)
		  (next-char))
		 ((find next-char plus-sign :test #'char=)
		  (setf sign #\+)
		  (next-char)))))
    ;; Integer part.
    (setf int (read-integer))
    (when (null next-char)
      (quit))
    ;; Optional decimal point.
    (when (find next-char decimal-point :test #'char=)
      (setf float t)
      ;; Skip decimal point.
      (next-char)
      ;; Fractional part.
      (let ((start digits))
	(setf frac (read-integer))
	(when (> digits start)
	  (setf frac (/ frac (expt 10 (- digits start)))))
	(when (null next-char)
	  (quit))))
    ;; Need at least one digit.
    (when (= digits 0)
      (quit))
    ;; Optional exponent part.
    (when (find next-char exponent-marker :test #'char=)
      (setf float t)
      ;; Start counting digits again.
      (setf digits 0)
      ;; Skip exponent marker.
      (next-char)
      ;; Exponent.
      (let ((sign #\+))
	(cond ((find next-char minus-sign :test #'char=)
	       (setf sign #\-)
	       (next-char))
	      ((find next-char plus-sign :test #'char=)
	       (setf sign #\+)
	       (next-char)))
	(setf exp (read-integer))
	(when (= digits 0)
	  (quit))
	(when (char= sign #\-)
	  (setf exp (- exp)))
	(when (null next-char)
	  (quit))))
    ))

;;; read-float.lisp ends here
