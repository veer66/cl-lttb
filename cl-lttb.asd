;;;; cl-lttb.asd

(asdf:defsystem #:cl-lttb
  :description "A Common Lisp wrapper for lttoolbox"
  :author "Vee Satayamas <vsatayamas@gmail.com>"
  :license "GPL-2.0"
  :version "0.0.1"
  :serial t
  :depends-on (#:cffi)
  :components ((:file "package")
               (:file "cl-lttb")))
