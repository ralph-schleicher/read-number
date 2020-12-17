;;; tests.lisp --- test procedure.

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

(in-package :common-lisp-user)

(quicklisp:quickload :lisp-unit)
(quicklisp:quickload :read-number)

(defpackage :read-number-tests
  (:use :common-lisp
	:lisp-unit
	:read-number))

(in-package :read-number-tests)

(defun try (fun spec &optional expected &key next-char)
  (multiple-value-bind (string arguments)
      (cond ((stringp spec)
	     (values spec nil))
	    (t
	     (values (first spec) (rest spec))))
    (with-input-from-string (input-stream string)
      (let (value status)
	(handler-case
	    (setf value (apply fun input-stream arguments)
		  status (= value expected))
	  (parse-error ()
	    (setf status (if (eq expected 'parse-error) t "failure (parse-error)")))
	  (end-of-file ()
	    (setf status (if (eq expected 'end-of-file) t "failure (end-of-file)")))
	  (t ()
	    (setf status "failure")))
	(values (and (eql status t)
		     (eql (peek-char nil input-stream nil) next-char))
		status value)
	))))

(define-test read-integer-test
  ;; Integers.
  (assert-true (try #'read-integer "0" 0))
  (assert-true (try #'read-integer "1" 1))
  (assert-true (try #'read-integer "42" 42))
  (assert-true (try #'read-integer "4223" 4223))
  ;; Different radix.
  (assert-true (try #'read-integer '("10" t nil nil :radix 2) #2r10))
  (assert-true (try #'read-integer '("21" t nil nil :radix 3) #3r21))
  (assert-true (try #'read-integer '("32" t nil nil :radix 4) #4r32))
  (assert-true (try #'read-integer '("43" t nil nil :radix 5) #5r43))
  (assert-true (try #'read-integer '("54" t nil nil :radix 6) #6r54))
  (assert-true (try #'read-integer '("65" t nil nil :radix 7) #7r65))
  (assert-true (try #'read-integer '("76" t nil nil :radix 8) #8r76))
  (assert-true (try #'read-integer '("87" t nil nil :radix 9) #9r87))
  (assert-true (try #'read-integer '("98" t nil nil :radix 10) #10r98))
  (assert-true (try #'read-integer '("a9" t nil nil :radix 11) #11ra9))
  (assert-true (try #'read-integer '("ba" t nil nil :radix 12) #12rba))
  (assert-true (try #'read-integer '("cb" t nil nil :radix 13) #13rcb))
  (assert-true (try #'read-integer '("dc" t nil nil :radix 14) #14rdc))
  (assert-true (try #'read-integer '("ed" t nil nil :radix 15) #15red))
  (assert-true (try #'read-integer '("fe" t nil nil :radix 16) #16rfe))
  (assert-true (try #'read-integer '("gf" t nil nil :radix 17) #17rgf))
  (assert-true (try #'read-integer '("hg" t nil nil :radix 18) #18rhg))
  (assert-true (try #'read-integer '("ih" t nil nil :radix 19) #19rih))
  (assert-true (try #'read-integer '("ji" t nil nil :radix 20) #20rji))
  (assert-true (try #'read-integer '("kj" t nil nil :radix 21) #21rkj))
  (assert-true (try #'read-integer '("lk" t nil nil :radix 22) #22rlk))
  (assert-true (try #'read-integer '("ml" t nil nil :radix 23) #23rml))
  (assert-true (try #'read-integer '("nm" t nil nil :radix 24) #24rnm))
  (assert-true (try #'read-integer '("on" t nil nil :radix 25) #25ron))
  (assert-true (try #'read-integer '("po" t nil nil :radix 26) #26rpo))
  (assert-true (try #'read-integer '("qp" t nil nil :radix 27) #27rqp))
  (assert-true (try #'read-integer '("rq" t nil nil :radix 28) #28rrq))
  (assert-true (try #'read-integer '("sr" t nil nil :radix 29) #29rsr))
  (assert-true (try #'read-integer '("ts" t nil nil :radix 30) #30rts))
  (assert-true (try #'read-integer '("ut" t nil nil :radix 31) #31rut))
  (assert-true (try #'read-integer '("vu" t nil nil :radix 32) #32rvu))
  (assert-true (try #'read-integer '("wv" t nil nil :radix 33) #33rwv))
  (assert-true (try #'read-integer '("xw" t nil nil :radix 34) #34rxw))
  (assert-true (try #'read-integer '("yx" t nil nil :radix 35) #35ryx))
  (assert-true (try #'read-integer '("zy" t nil nil :radix 36) #36rzy))
  (assert-true (try #'read-integer '("ZY" t nil nil :radix 36) #36rzy))
  ;; Leading zeros.
  (assert-true (try #'read-integer "00" 0))
  (assert-true (try #'read-integer "007" 7))
  ;; Signed numbers.
  (assert-true (try #'read-integer "+0" 0))
  (assert-true (try #'read-integer "-0" 0))
  (assert-true (try #'read-integer "+42" 42))
  (assert-true (try #'read-integer "-42" -42))
  ;; Trailing characters.
  (assert-true (try #'read-integer "42 " 42 :next-char #\Space))
  (assert-true (try #'read-integer "42|" 42 :next-char #\|))
  ;; Errors.
  (assert-true (try #'read-integer "" 'end-of-file))
  (assert-true (try #'read-integer "+" 'end-of-file))
  (assert-true (try #'read-integer "-" 'end-of-file))
  (assert-true (try #'read-integer "|" 'parse-error :next-char #\|))
  (assert-true (try #'read-integer "+|" 'parse-error :next-char #\|))
  (assert-true (try #'read-integer "-|" 'parse-error :next-char #\|))
  (assert-true (try #'read-integer " 42" 'parse-error :next-char #\Space))
  (assert-true (try #'read-integer '("+42" t nil nil :unsigned-number t) 'parse-error :next-char #\+))
  (assert-true (try #'read-integer '("-42" t nil nil :unsigned-number t) 'parse-error :next-char #\-))
  (assert-true (try #'read-integer '("+42" t nil nil :unsigned-number :plus) 'parse-error :next-char #\+))
  (assert-true (try #'read-integer '("-42" t nil nil :unsigned-number :plus) -42))
  ;; Digit groups.
  (assert-true (try #'read-integer '("100_000" t nil nil :group-separator "_") 100000))
  (assert-true (try #'read-integer '("10_00_00" t nil nil :group-separator "_") 100000))
  (assert-true (try #'read-integer '("1_0_0_0_0_0" t nil nil :group-separator "_") 100000))
  (assert-true (try #'read-integer '("100_000_" t nil nil :group-separator "_") 'end-of-file))
  (assert-true (try #'read-integer '("100_000_|" t nil nil :group-separator "_") 'parse-error :next-char #\|))
  (assert-true (try #'read-integer '("100__000|" t nil nil :group-separator "_") 'parse-error :next-char #\_))
  (assert-true (try #'read-integer '("_100_000|" t nil nil :group-separator "_") 'parse-error :next-char #\_))
  (assert-true (try #'read-integer '("+_100_000" t nil nil :group-separator "_") 'parse-error :next-char #\_))
  (assert-true (try #'read-integer '("-_100_000" t nil nil :group-separator "_") 'parse-error :next-char #\_))
  (assert-true (try #'read-integer '("10'00_00" t nil nil :group-separator "_'") 1000 :next-char #\_))
  (values))

(define-test read-float-test
  ;; Integers.
  (assert-true (try #'read-float "0" 0))
  (assert-true (try #'read-float "1" 1))
  (assert-true (try #'read-float "42" 42))
  (assert-true (try #'read-float "4223" 4223))
  ;; Leading zeros.
  (assert-true (try #'read-float "00" 0))
  (assert-true (try #'read-float "007" 7))
  ;; Signed numbers.
  (assert-true (try #'read-float "+0" 0))
  (assert-true (try #'read-float "-0" 0))
  (assert-true (try #'read-float "+42" 42))
  (assert-true (try #'read-float "-42" -42))
  ;; Decimal numbers.
  (assert-true (try #'read-float "42." 42.0))
  (assert-true (try #'read-float "42.0" 42.0))
  (assert-true (try #'read-float "42.00" 42.0))
  (assert-true (try #'read-float "42.23" 42.23))
  (assert-true (try #'read-float "42.230" 42.23))
  (assert-true (try #'read-float "42.2300" 42.23))
  (assert-true (try #'read-float ".23" 0.23))
  (assert-true (try #'read-float "+.23" 0.23))
  (assert-true (try #'read-float "-.23" -0.23))
  ;; Exponential notation.
  (assert-true (try #'read-float "1E1" 1E1))
  (assert-true (try #'read-float "+1E1" 1E1))
  (assert-true (try #'read-float "-1E1" -1E1))
  (assert-true (try #'read-float "1.E1" 1E1))
  (assert-true (try #'read-float "1.0E1" 1E1))
  (assert-true (try #'read-float ".1E2" 0.1E2))
  (assert-true (try #'read-float "1E+5" 1E+5))
  (assert-true (try #'read-float "1E-5" 1E-5))
  (assert-true (try #'read-float "1E00" 1E0))
  (assert-true (try #'read-float "1E01" 1E1))
  (assert-true (try #'read-float "1E+01" 1E+1))
  (assert-true (try #'read-float "1E-01" 1E-1))
  ;; Trailing characters.
  (assert-true (try #'read-float "42 " 42 :next-char #\Space))
  (assert-true (try #'read-float "42|" 42 :next-char #\|))
  ;; Errors.
  (assert-true (try #'read-float "" 'end-of-file))
  (assert-true (try #'read-float "+" 'end-of-file))
  (assert-true (try #'read-float "-" 'end-of-file))
  (assert-true (try #'read-float "." 'end-of-file))
  (assert-true (try #'read-float "1E" 'end-of-file))
  (assert-true (try #'read-float "1E+" 'end-of-file))
  (assert-true (try #'read-float "1E-" 'end-of-file))
  (assert-true (try #'read-float "E" 'parse-error :next-char #\E))
  (assert-true (try #'read-float "+E" 'parse-error :next-char #\E))
  (assert-true (try #'read-float "-E" 'parse-error :next-char #\E))
  (assert-true (try #'read-float ".E" 'parse-error :next-char #\E))
  (assert-true (try #'read-float "|" 'parse-error :next-char #\|))
  (assert-true (try #'read-float "+|" 'parse-error :next-char #\|))
  (assert-true (try #'read-float "-|" 'parse-error :next-char #\|))
  (assert-true (try #'read-float ".|" 'parse-error :next-char #\|))
  (assert-true (try #'read-float "1E|" 'parse-error :next-char #\|))
  (assert-true (try #'read-float "1E+|" 'parse-error :next-char #\|))
  (assert-true (try #'read-float "1E-|" 'parse-error :next-char #\|))
  (assert-true (try #'read-float " 42" 'parse-error :next-char #\Space))
  (assert-true (try #'read-float '("+42" t nil nil :unsigned-number t) 'parse-error :next-char #\+))
  (assert-true (try #'read-float '("-42" t nil nil :unsigned-number t) 'parse-error :next-char #\-))
  (assert-true (try #'read-float '("+42" t nil nil :unsigned-number :plus) 'parse-error :next-char #\+))
  (assert-true (try #'read-float '("-42" t nil nil :unsigned-number :plus) -42))
  ;; Digit groups.
  (assert-true (try #'read-float '("100_000" t nil nil :group-separator "_") 100000))
  (assert-true (try #'read-float '("10_00_00" t nil nil :group-separator "_") 100000))
  (assert-true (try #'read-float '("1_0_0_0_0_0" t nil nil :group-separator "_") 100000))
  (assert-true (try #'read-float '("100_000_" t nil nil :group-separator "_") 'end-of-file))
  (assert-true (try #'read-float '("100_000_|" t nil nil :group-separator "_") 'parse-error :next-char #\|))
  (assert-true (try #'read-float '("100__000|" t nil nil :group-separator "_") 'parse-error :next-char #\_))
  (assert-true (try #'read-float '("_100_000|" t nil nil :group-separator "_") 'parse-error :next-char #\_))
  (assert-true (try #'read-float '("+_100_000" t nil nil :group-separator "_") 'parse-error :next-char #\_))
  (assert-true (try #'read-float '("-_100_000" t nil nil :group-separator "_") 'parse-error :next-char #\_))
  (assert-true (try #'read-float '("10'00_00" t nil nil :group-separator "_'") 1000 :next-char #\_))
  (assert-true (try #'read-float '("0.000_005" t nil nil :group-separator "_") 0.000005))
  (assert-true (try #'read-float '("0.00_00_05" t nil nil :group-separator "_") 0.000005))
  (assert-true (try #'read-float '("0.0_0_0_0_0_5" t nil nil :group-separator "_") 0.000005))
  (assert-true (try #'read-float '("0.000_005_" t nil nil :group-separator "_") 'end-of-file))
  (assert-true (try #'read-float '("0.000_005_|" t nil nil :group-separator "_") 'parse-error :next-char #\|))
  (assert-true (try #'read-float '("0.000__005|" t nil nil :group-separator "_") 'parse-error :next-char #\_))
  (assert-true (try #'read-float '("0._000_005|" t nil nil :group-separator "_") 0 :next-char #\_))
  (assert-true (try #'read-float '("10_00.00_05" t nil nil :group-separator "_") 1000.0005))
  (assert-true (try #'read-float '("10_00_.00_05" t nil nil :group-separator "_") 'parse-error :next-char #\.))
  (assert-true (try #'read-float '("10_00._00_05" t nil nil :group-separator "_") 1000 :next-char #\_))
  (values))

(run-tests)

;;; tests.lisp ends here
