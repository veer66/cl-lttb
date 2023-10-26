;;;; cl-lttb.lisp

(in-package #:cl-lttb)

(define-foreign-library clttoolbox (t (:default "libclttoolbox")))
(use-foreign-library clttoolbox)

(defcfun ("lttb_fst_processor_new" fst-processor-new) :pointer)
(defcfun ("lttb_fst_processor_destroy" fst-processor-destroy) :void (fst_processor :pointer))
(defcfun ("lttb_fst_processor_load" fst-processor-load*) :void (fst_processor :pointer) (input_pathname :pointer))
(defun fst-processor-load (fst-processor input-pathname)
  (declare (pathname input-pathname))
  (with-foreign-string (input-pathname-c (namestring input-pathname))
    (fst-processor-load* fst-processor input-pathname-c)))

(defcfun ("lttb_fst_processor_is_valid" fst-processor-is-valid*) :int (fst_processor :pointer))
(defun fst-processor-valid-p (fst-processor)
  (not (= 0 (fst-processor-is-valid* fst-processor))))

(defcenum generation-mode
  :gm_clean
  :gm_unknown
  :gm_all
  :gm_tagged
  :gm_tagged_nm
  :gm_carefulcase)

(defcfun ("lttb_fst_processor_generate" fst-processor-generate*)
  :void
  (fst_processor :pointer)
  (input_pathname :pointer)
  (output_pathname :pointer)
  (mode :int))

(defun fst-processor-generate (fst-processor input-pathname output-pathname mode)
  (let ((input-pathname-c (if input-pathname
			      (foreign-string-alloc (namestring input-pathname))
			      (null-pointer)))
	(output-pathname-c (if output-pathname
			       (foreign-string-alloc (namestring output-pathname))
			       (null-pointer)))
	(mode-c (foreign-enum-value 'generation-mode mode)))
    (fst-processor-generate* fst-processor input-pathname-c output-pathname-c mode-c)
    (unless (null-pointer-p input-pathname-c)
      (foreign-string-free input-pathname-c))
    (unless (null-pointer-p output-pathname-c)
      (foreign-string-free output-pathname-c))))
