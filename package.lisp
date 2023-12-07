;;;; package.lisp

(defpackage #:cl-lttb
  (:use #:cl #:cffi)
  (:export #:fst-processor-new
	   #:fst-processor-destroy
	   #:fst-processor-load
	   #:fst-processor-valid-p
	   #:fst-processor-init-analysis
	   #:fst-processor-init-generation
	   #:fst-processor-init-postgeneration
	   #:fst-processor-analysis
	   #:fst-processor-analysis-in-mem
	   #:fst-processor-generate
	   #:fst-processor-generate-in-mem
	   #:fst-processor-postgenerate
	   #:fst-processor-postgenerate-in-mem
	   #:with-fst-generator
	   #:with-fst-postgenerator
	   #:unable-to-load-fst))
