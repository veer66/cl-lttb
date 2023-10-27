;;;; cl-lttb.asd

(asdf:defsystem #:cl-lttb
  :description "A Common Lisp wrapper for lttoolbox"
  :author "Vee Satayamas <vsatayamas@gmail.com>"
  :license "GPL-2.0"
  :version "0.0.1"
  :serial t
  :depends-on (#:cffi)
  :components ((:file "package")
               (:file "cl-lttb"))
  :in-order-to ((test-op (test-op "cl-lttb/tests"))))

(asdf:defsystem "cl-lttb/tests" 
  :depends-on ("cl-lttb" "fiveam")
  :components ((:module "t"
		:components ((:file "tests"))))
  :perform (test-op (o s)
		    (uiop:symbol-call :fiveam '#:run!
				      (uiop:find-symbol* 'fst-processor-suite 'cl-lttb-tests))))
