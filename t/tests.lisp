(defpackage :cl-lttb-tests
  (:use :cl :fiveam :cl-lttb))

(in-package :cl-lttb-tests)

(def-suite fst-processor-suite)
(in-suite fst-processor-suite)


(test fst-processor-generate-this-product
  (let* ((pid (osicat-posix:getpid))
	 (tmp-imm-pathname (make-pathname :directory "tmp"
					  :name (format nil "cl-lttb-test-fst-gen-~a" pid)))
	 (tmp-final-pathname (make-pathname :directory "tmp"
					    :name (format nil "cl-lttb-test-fst-final-~a" pid)))
	 (fst-processor (fst-processor-new))
	 (fst-processor-post (fst-processor-new)))
    (fst-processor-load fst-processor #P"t/tha-eng.autogen.bin")
    (fst-processor-init-generation fst-processor)
    (fst-processor-load fst-processor-post #P"t/tha-eng.autopgen.bin")
    (fst-processor-init-postgeneration fst-processor-post)
    (is (fst-processor-valid-p fst-processor))
    (fst-processor-generate fst-processor "t/morph1.txt" tmp-imm-pathname :GM_CLEAN)
    (is (equal "this product"
	       (with-open-file (f tmp-imm-pathname)
		 (read-line f))))
    (fst-processor-postgenerate fst-processor-post tmp-imm-pathname tmp-final-pathname)
    (is (equal "this product"
	       (with-open-file (f tmp-final-pathname)
		 (read-line f))))
    (uiop:delete-file-if-exists tmp-imm-pathname)
    (uiop:delete-file-if-exists tmp-final-pathname)
    (fst-processor-destroy fst-processor-post)
    (fst-processor-destroy fst-processor)))

(test with-generator-this-product
  (with-fst-generator (g #P"t/tha-eng.autogen.bin")
    (is (equal "this product"
	       (fst-processor-generate-in-mem g "^this<det><dem><sg>$ ^product<n><sg>$" 0)))))
