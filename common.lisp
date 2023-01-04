;;; common.lisp --- common definitions

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

;;; Commentary:

;; The portable way to encode an Unicode character is by using the
;; Babel library.  For example:
;;
;;      (defun string-from-octets (sequence &optional (encoding :UTF-8))
;;        (babel:octets-to-string
;;         (coerce sequence '(vector (unsigned-byte 8)))
;;         :encoding encoding))
;;
;;      ;; Unicode U+2212, minus sign.
;;      (string-from-octets #(226 136 146))
;;
;; The following table lists various encodings of some interesting
;; characters.
;;
;; | Unicode | UTF-8 Octects |           Lisp Reader           |
;; |---------|---------------|---------------------------------|
;; | U+002B  |   43          | #\+ | #\PLUS_SIGN               |
;; | U+002D  |   45          | #\- | #\HYPHEN-MINUS            |
;; | U+2212  |  226 136 146  |     | #\MINUS_SIGN              |
;; | U+0027  |   39          | #\' | #\APOSTROPHE              |
;; | U+005F  |   95          | #\_ | #\LOW_LINE                |
;; | U+00A0  |  194 160      |     | #\NO-BREAK_SPACE          |
;; | U+202F  |  226 128 175  |     | #\NARROW_NO-BREAK_SPACE   |
;; | U+23E8  |  226 143 168  |     | #\DECIMAL_EXPONENT_SYMBOL |

;;; Code:

(in-package :read-number)

(declaim (inline sequence-of-characters-p))
(defun sequence-of-characters-p (object)
  "Return true if OBJECT is a sequence of characters.
If OBJECT is an empty sequence, value is true, too."
  (every #'characterp object))

(deftype sequence-of-characters ()
  "Type specifier for a sequence of characters."
  '(satisfies sequence-of-characters-p))

(defvar *default-plus-sign* "+"
  "Controls the set of valid plus sign characters when reading an
external number representation and no explicit plus sign keyword
argument is specified.  Value has to be a sequence of characters.
The default is ‘+’ (Unicode U+002B, plus sign).")
(declaim (type sequence-of-characters *default-plus-sign*))

(defvar *default-minus-sign* "-"
  "Controls the set of valid minus sign characters when reading an
external number representation and no explicit minus sign keyword
argument is specified.  Value has to be a sequence of characters.
The default is ‘-’ (Unicode U+002D, hyphen-minus).

Another candidate for this character set is ‘−’ (Unicode U+2212,
minus sign).")
(declaim (type sequence-of-characters *default-minus-sign*))

(defvar *default-group-separator* ()
  "Controls the set of valid group separator characters when reading
an external number representation and no explicit group separator
keyword argument is specified.  Value has to be a sequence of
characters.  The default is the empty set.

Candidates for this character set are ‘'’ (Unicode U+0027, apostrophe),
‘_’ (Unicode U+005F, low line), ‘ ’ (Unicode U+00A0, no-break space),
and ‘ ’ (Unicode U+202F, narrow no-break space).  The preferred group
separator according to ISO 31 is the narrow no-break space character.")
(declaim (type sequence-of-characters *default-group-separator*))

(defvar *default-decimal-point* "."
  "Controls the set of valid decimal point (or better radix point)
characters when reading an external number representation and no
explicit decimal point keyword argument is specified.  Value has to
be a sequence of characters.  The default is ‘.’ (Unicode U+002E,
full stop).

Another candidate for this character set is ‘,’ (Unicode U+002C,
comma).  ISO 31 uses the decimal point in the international English
version of the standard and the decimal comma in the original French
version.  Interactive applications should accept both variants to
improve usability.")
(declaim (type sequence-of-characters *default-decimal-point*))

(defvar *default-exponent-marker* "EeDd" ;f-language
  "Controls the set of valid exponent marker characters when reading
an external number representation and no explicit exponent marker
keyword argument is specified.  Value has to be a sequence of
characters.  The default is ‘E’, ‘e’, ‘D’, and ‘d’.

Another candidate for this character set is ‘⏨’ (Unicode U+23E8,
decimal exponent symbol).")
(declaim (type sequence-of-characters *default-exponent-marker*))

(declaim (inline standard-digit-char-p))
(defun standard-digit-char-p (char &optional (radix 10))
  "Return true if CHAR is a standard digit character.

First argument CHAR has to be a character object.
Optional second argument RADIX is an integer between 2 and 36,
 inclusive.  The default is 10.

Return value is the weight of CHAR as an integer, or nil."
  (check-type char character)
  (and (standard-char-p char)
       (digit-char-p char radix)))

(defmacro with-input-from ((input-stream eof-error-p eof-value recursivep) (bindings result) &body body)
  "Framework for reading numbers.  The local bindings available in BODY
are documented below.

 -- next-char (&optional (EOF-QUIT-P t))                      [Function]
     Read the next character from INPUT-STREAM.  If optional
     argument EOF-QUIT-P is true, call ‘quit’ if no character
     can be read.  Modifies ‘next-char’ and maybe ‘length’.

 -- next-char                                                 [Variable]
     The last character read by the ‘next-char’ function.

 -- quit ()                                                   [Function]
     Undo the effects of the ‘next-char’ function.  Return RESULT
     if a valid number could be read.  Otherwise, signal an error.

 -- length                                                    [Variable]
     The total number of characters read so far.  This is the
     secondary value returned by the ‘quit’ function.  This
     variable shall be considered read-only.

 -- digits                                                    [Variable]
     The total number of digits consumed so far.  This variable
     can be modified by the user.  A value of zero indicates for
     the ‘quit’ function that no valid number could be read.

 -- read-int (RADIX GROUP-SEPARATOR)                          [Function]
     Read a sequence of digits and return its numerical value.
     May call ‘next-char’ and ‘quit’.  Maybe modifies ‘digits’."
  (alexandria:once-only (input-stream eof-error-p eof-value recursivep
			 ;; The actual group separator.
			 (group-separator-char nil)
			 ;; The global parser state, non-null indicates
			 ;; a parse error.
			 (state nil))
    `(prog (next-char
	    (length 0)
	    (digits 0)
	    ,@bindings)
	(labels ((next-char (&optional (eof-quit-p t))
		   "Read the next character from INPUT-STREAM."
		   (setf next-char (read-char ,input-stream nil nil ,recursivep))
		   (if (null next-char)
		       (when eof-quit-p
			 (quit))
		     (incf length))
		   next-char)
		 (quit ()
		   "Return the resulting number, or signal an error."
		   (when next-char
		     (unread-char next-char ,input-stream)
		     (decf length))
		   (when (or (= digits 0) ,state)
		     (when next-char
		       (error 'parse-error :stream ,input-stream))
		     ;; Always signal an end-of-file error when the file
		     ;; ends in the middle of an object.
		     (when (or (> length 0) ,eof-error-p)
		       (error 'end-of-file :stream ,input-stream))
		     (return (values ,eof-value length)))
		   (return (values ,result length)))
		 (read-int (radix group-separator)
		   "Read a sequence of digits."
		   (check-type radix (integer 2 36))
		   (let ((value 0)
			 ;; Save global parser state.  Will be
			 ;; restored if no parse error occurs.
			 (state ,state))
		     (setf ,state nil)
		     (let (digit)
		       (loop (cond ((and (eq ,state :digit)
					 (if (not (null ,group-separator-char))
					     (char= next-char ,group-separator-char)
					   (when (find next-char group-separator :test #'char=)
					     (setf ,group-separator-char next-char))))
				    (setf ,state :group-separator)
				    (next-char))
				   (t
				    (setf digit (standard-digit-char-p next-char radix))
				    (when (null digit)
				      (return))
				    (setf ,state :digit)
				    (setf value (+ (* value radix) digit))
				    (incf digits)
				    (next-char nil)
				    (when (null next-char)
				      (return))))))
		     (when (eq ,state :group-separator)
		       (quit))
		     (setf ,state state)
		     value)))
	  ;; Read first character.
	  (next-char)
	  ;; Parse the number.
	  ,@body
	  ;; Done.
	  (quit)))))

(defvar *calculation-timeout* 0
  "Period of time allowed to elapse before a calculation is aborted.
Value has to be a non-negative number.  A value of zero means to
disable timeouts, i.e. a calculation can take any time.  This is
the default.  Time is measured in seconds.

A calculation timeout is a safety mechanism when reading a number
from an untrustworthy source.  For example, the number 1E+1000000
is syntactically correct but requires quite some CPU cycles to
represent it as a Lisp object.

If a calculation timeout occurs, an error of type ‘arithmetic-error’
is signaled.")
(declaim (type (real 0) *calculation-timeout*))

#+sbcl
(defmacro with-timeout (seconds form &body handler)
  `(handler-case
       (sb-ext:with-timeout ,seconds ,form)
     (sb-ext:timeout ()
       ,@handler)))

#-sbcl
(if (fboundp 'bordeaux-threads:with-timeout)
    (defmacro with-timeout (seconds form &body handler)
      (alexandria:once-only (seconds)
	`(flet ((form-body () ,form))
	   (if (plusp ,seconds)
	       (handler-case
		   (bordeaux-threads:with-timeout (,seconds)
		     (form-body))
		 (bordeaux-threads:timeout ()
		   ,@handler))
	     (form-body)))))
  ;; No implementation.
  (defmacro with-timeout (seconds form &body handler)
    form))

(setf (documentation 'with-timeout 'function)
      "Execute a form within a specified period of time.

First argument SECONDS is the period of time allowed to elapse before
 the execution of the form is aborted.  Value has to be a non-negative
 number.  A value of zero disables the timeout.
Second argument FORM is the form to be executed.
Optional third argument HANDLER is the timeout handler.  It will be
 executed when the form does not finish in time.

Return either the values of FORM or the values of HANDLER.")

;;; common.lisp ends here
