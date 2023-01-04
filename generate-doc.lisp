;;; generate-doc.lisp --- generate documentation

;; Copyright (C) 2015 Ralph Schleicher

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

(quicklisp:quickload :read-number)
(quicklisp:quickload :rs-doc) ;private

(let ((symbols '(read-number:*default-plus-sign*
		 read-number:*default-minus-sign*
		 read-number:*default-group-separator*
		 read-number:*default-decimal-point*
		 read-number:*default-exponent-marker*
		 read-number:*calculation-timeout*
		 read-number:read-integer
		 read-number:read-float)))
  (rs-doc:generate-doc
   :package :read-number
   :symbols symbols 
   :output-format :html
   :output (make-pathname :directory '(:relative "doc")
			  :name "read-number"
			  :type "html"))
  (rs-doc:generate-doc
   :package :read-number
   :symbols symbols
   :output-format :text
   :output (make-pathname :directory '(:relative "doc")
			  :name "read-number"
			  :type "txt"))
  (values))

;;; generate-doc.lisp ends here
