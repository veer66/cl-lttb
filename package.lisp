;;;; package.lisp

(defpackage #:cl-lttb
  (:use #:cl #:cffi)
  (:export #:fst-processor-new
	   #:fst-processor-destroy
	   #:fst-processor-load
	   #:fst-processor-valid-p
	   #:fst-processor-init-generation
	   #:fst-processor-init-postgeneration
	   #:fst-processor-generate
	   #:fst-processor-generate-in-mem
	   #:fst-processor-postgenerate
	   #:with-fst-generator))
