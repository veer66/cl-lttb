(defpackage :cl-lttb-tests
  (:use :cl :fiveam))

(in-package :cl-lttb-tests)

(def-suite fst-processor-suite)
(in-suite fst-processor-suite)

(test fst-processor-generate-this-product
  (is (eq :x :x)))

;;(run!)

