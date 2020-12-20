;;; read-integer.lisp --- read an integral number.

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

(defun read-integer (&optional
		       (input-stream *standard-input*)
		       (eof-error-p t)
		       eof-value
		       recursivep
		     &key
		       unsigned-number
		       (plus-sign *default-plus-sign*)
		       (minus-sign *default-minus-sign*)
		       (group-separator *default-group-separator*)
		       (radix 10))
  "Read an integer from an input stream.

Optional first argument INPUT-STREAM is an input stream designator.
 The default is standard input.
Optional second argument EOF-ERROR-P is a generalized boolean.
 If an end of file error occurs and EOF-ERROR-P is false, EOF-VALUE
 is returned.  Otherwise, signal an ‘end-of-file’ error.  The default
 is true.
Optional third argument EOF-VALUE is an object.  See above for more
 details.  The default is nil.
Optional fourth argument RECURSIVEP is a generalized boolean.  True
 means that this call is expected to be embedded in a higher-level
 call to ‘read’ or a similar function used by the Lisp reader.  The
 default is false.
Keyword argument UNSIGNED-NUMBER is a generalized boolean.  True means
 to read an unsigned positive number.  If UNSIGNED-NUMBER is ‘:plus’,
 an explicit plus sign character signals a ‘parse-error’ but negative
 numbers are permitted.  The default is false.
Keyword argument PLUS-SIGN is a sequence of valid plus sign
 characters.  The plus sign is used to denote a positive number.
 The default is the value of the ‘*default-plus-sign*’ special
 variable.
Keyword argument MINUS-SIGN is a sequence of valid minus sign
 characters.  The minus sign is used to denote a negative number.
 The default is the value of the ‘*default-minus-sign*’ special
 variable.
Keyword argument GROUP-SEPARATOR is a sequence of valid group
 separator characters.  The group separator is used to separate the
 digits of a number into groups.  The default is the value of the
 ‘*default-group-separator*’ special variable.  The group separator
 of a number can not change, i.e. the first matching group separator
 fixes the group separator for the rest of the number.
Keyword argument RADIX is a radix.  Value has to be an integer between
 2 and 36, inclusive.  The default is 10.  Case is not significant for
 the digit characters ‘A’ to ‘Z’ when parsing numbers with a radix
 greater than 10.

Return value is an integer.  Secondary value is the number of
characters read.

The ‘read-integer’ function expects an optional sign followed by a
non-empty sequence of digits in the specified radix.  It does not
recognize a decimal point or exponent marker.  Leading or trailing
whitespace is not ignored.

If the file ends in the middle of an integer representation,
‘read-integer’ signals an ‘end-of-file’ error regardless of
the value of the EOF-ERROR-P argument.

The result if undefined if the sequences of valid plus and minus
sign characters intersect."
  (check-type radix (integer 2 36))
  (with-input-from (input-stream eof-error-p eof-value recursivep)
      (;; Bindings.
       ((sign #\+)
	(int 0))
       ;; Result.
       (if (char= sign #\-) (- int) int))
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
    (setf int (read-int radix group-separator))
    ))

;;; read-integer.lisp ends here
