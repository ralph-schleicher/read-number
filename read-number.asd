;;; read-number.asd --- reading numbers from an input stream

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

(in-package :asdf-user)

(defsystem "read-number"
  :description "Reading numbers from an input stream."
  :author "Ralph Schleicher <rs@ralph-schleicher.de>"
  :license "Modified BSD License"
  :version (:read-file-line "VERSION")
  :depends-on ("alexandria")
  :serial t
  :components ((:file "packages")
	       (:file "common")
	       (:file "read-integer")
	       (:file "read-float"))
  :in-order-to ((test-op (test-op "read-number/tests"))))

(defsystem "read-number/tests"
  :description "Test suite for the ‘read-number’ system."
  :author "Ralph Schleicher <rs@ralph-schleicher.de>"
  :license "Modified BSD License"
  :depends-on ("lisp-unit"
               "read-number")
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :read-number-tests :main)))

;;; read-number.asd ends here
