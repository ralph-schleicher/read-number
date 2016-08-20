;;; common.lisp --- common definitions.

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

(declaim (inline sequence-of-characters-p))
(defun sequence-of-characters-p (object)
  "Return true if OBJECT is a sequence of characters.
If OBJECT is an empty sequence, value is true, too."
  (every #'characterp object))

(deftype sequence-of-characters ()
  "Type specifier for a sequence of characters."
  '(satisfies sequence-of-characters-p))

(defvar *default-plus-sign* "+"
  "Sequence of valid plus sign characters.")
(declaim (type sequence-of-characters *default-plus-sign*))

(defvar *default-minus-sign* "-"
  "Sequence of valid minus sign characters.")
(declaim (type sequence-of-characters *default-minus-sign*))

(defvar *default-decimal-point* "."
  "Sequence of valid decimal point characters.")
(declaim (type sequence-of-characters *default-decimal-point*))

(defvar *default-exponent-marker* "EeDd"
  "Sequence of valid exponent marker characters.")
(declaim (type sequence-of-characters *default-exponent-marker*))

(declaim (inline standard-digit-char-p))
(defun standard-digit-char-p (char &optional (radix 10))
  "Return true if CHAR is a standard digit character.

First argument CHAR has to be a character object.
Optional second argument RADIX is an integer between 2 and 36,
 inclusive.  Default is 10.

Return value is the weight of CHAR as an integer, or nil."
  (check-type char character)
  (and (standard-char-p char)
       (digit-char-p char radix)))

(defmacro with-input-from ((input-stream eof-error-p eof-value recursive-p) (bindings result) &body body)
  "Framework for reading numbers."
  (alexandria:once-only (input-stream eof-error-p eof-value recursive-p)
    `(prog (next-char
	    (length 0)
	    (digits 0)
	    ,@bindings)
	(labels ((next-char (&optional (eof-quit-p t))
		   "Read the next character from INPUT-STREAM."
		   (setf next-char (read-char ,input-stream nil nil ,recursive-p))
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
		   (when (= digits 0)
		     (when next-char
		       (error 'parse-error :stream ,input-stream))
		     ;; Always signal an end-of-file error when the file
		     ;; ends in the middle of an object.
		     (when (or (> length 0) ,eof-error-p)
		       (error 'end-of-file :stream ,input-stream))
		     (return ,eof-value))
		   (return ,result))
		 (read-integer (&optional (radix 10))
		   "Read an integral number."
		   (check-type radix (integer 2 36))
		   (let ((value 0))
		     (let (digit)
		       (loop (setf digit (standard-digit-char-p next-char radix))
			     (when (null digit)
			       (return))
			     (setf value (+ (* value radix) digit))
			     (incf digits)
			     (next-char nil)
			     (when (null next-char)
			       (return))))
		     value)))
	  ;; Read first character.
	  (next-char)
	  ;; Parse the number.
	  ,@body
	  ;; Done.
	  (quit)))))

;;; common.lisp ends here
