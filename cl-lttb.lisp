;;;; cl-lttb.lisp

(in-package #:cl-lttb)

(defcenum lttb-error-code
  :lttb-ok
  :lttb-cannot-open-input-file
  :lttb-cannot-open-output-file)

(define-condition unable-to-load-fst (error)
  ((fst-pathname :initarg :fst-pathname :reader fst-pathname)))

(define-foreign-library clttoolbox (t (:default "libclttoolbox")))
(use-foreign-library clttoolbox)

(defcfun ("lttb_fst_processor_new" fst-processor-new) :pointer)
(defcfun ("lttb_fst_processor_destroy" fst-processor-destroy) :void (fst_processor :pointer))
(defcfun ("lttb_fst_processor_load" fst-processor-load*) :int (fst_processor :pointer) (input_pathname :pointer))

(defun fst-processor-load (fst-processor input-pathname)
  (declare (pathname input-pathname))
  (unless (eq (with-foreign-string (input-pathname-c (namestring input-pathname))
		(fst-processor-load* fst-processor input-pathname-c))
	      (foreign-enum-value 'lttb-error-code :lttb-ok))
    (error 'unable-to-load-fst :fst-pathname input-pathname)))

(defcfun ("lttb_fst_processor_is_valid" fst-processor-is-valid*) :int (fst_processor :pointer))
(defun fst-processor-valid-p (fst-processor)
  (not (= 0 (fst-processor-is-valid* fst-processor))))

(defcfun ("lttb_fst_processor_init_analysis" fst-processor-init-analysis) :void (fst_processor :pointer))
(defcfun ("lttb_fst_processor_init_generation" fst-processor-init-generation) :void (fst_processor :pointer))
(defcfun ("lttb_fst_processor_init_postgeneration" fst-processor-init-postgeneration) :void (fst_processor :pointer))

(defcfun ("lttb_fst_processor_analysis" fst-processor-analysis*)
  :void
  (fst_processor :pointer)
  (input_pathname :pointer)
  (output_pathname :pointer))

(defun fst-processor-analysis (fst-processor input-pathname output-pathname)
  (let ((input-pathname-c (if input-pathname
			      (foreign-string-alloc (namestring input-pathname))
			      (null-pointer)))
	(output-pathname-c (if output-pathname
			       (foreign-string-alloc (namestring output-pathname))
			       (null-pointer))))
    (fst-processor-analysis* fst-processor input-pathname-c output-pathname-c)
    (unless (null-pointer-p input-pathname-c)
      (foreign-string-free input-pathname-c))
    (unless (null-pointer-p output-pathname-c)
      (foreign-string-free output-pathname-c))))

(defcfun ("lttb_fst_processor_analysis_in_mem" fst-processor-analysis-in-mem*)
  :int
  (fst_processor :pointer)
  (input_buf :pointer)
  (output_buf :pointer)
  (output_buf_size :pointer))

(defun fst-processor-analysis-in-mem (fst-processor input-string)
  (with-foreign-string (input-string* input-string)
    (let ((output-buffer-pointer (cffi:foreign-alloc :pointer))
	  (size-pointer (cffi:foreign-alloc :pointer)))
      (cl-lttb::fst-processor-analysis-in-mem* fst-processor
					       input-string*
					       output-buffer-pointer
					       size-pointer)
      (let* ((size (cffi:mem-ref size-pointer :int))
	     (output-buffer-pointer* (cffi:mem-ref output-buffer-pointer :pointer))
	     (output-buffer (cffi:foreign-string-to-lisp output-buffer-pointer* :count size)))
	(cffi:foreign-free output-buffer-pointer)
	(cffi:foreign-free size-pointer)
	output-buffer))))

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

(defcfun ("lttb_fst_processor_generate_in_mem" fst-processor-generate-in-mem*)
  :int
  (fst_processor :pointer)
  (input_buf :pointer)
  (output_buf :pointer)
  (output_buf_size :pointer)
  (mode :int))

(defun fst-processor-generate-in-mem (fst-processor input-string mode)
  (let ((mode-c (foreign-enum-value 'generation-mode mode)))
    (with-foreign-string (input-string* input-string)
      (let ((output-buffer-pointer (cffi:foreign-alloc :pointer))
	    (size-pointer (cffi:foreign-alloc :pointer)))
	(cl-lttb::fst-processor-generate-in-mem* fst-processor
						 input-string*
						 output-buffer-pointer
						 size-pointer
						 mode-c)
	(let* ((size (cffi:mem-ref size-pointer :int))
	       (output-buffer-pointer* (cffi:mem-ref output-buffer-pointer :pointer))
	       (output-buffer (cffi:foreign-string-to-lisp output-buffer-pointer* :count size)))
	  (cffi:foreign-free output-buffer-pointer)
	  (cffi:foreign-free size-pointer)
	  output-buffer)))))

(defcfun ("lttb_fst_processor_postgenerate" fst-processor-postgenerate*)
  :void
  (fst_processor :pointer)
  (input_pathname :pointer)
  (output_pathname :pointer))

(defun fst-processor-postgenerate (fst-processor input-pathname output-pathname)
  (let ((input-pathname-c (if input-pathname
			      (foreign-string-alloc (namestring input-pathname))
			      (null-pointer)))
	(output-pathname-c (if output-pathname
			       (foreign-string-alloc (namestring output-pathname))
			       (null-pointer))))
    (fst-processor-postgenerate* fst-processor input-pathname-c output-pathname-c)
    (unless (null-pointer-p input-pathname-c)
      (foreign-string-free input-pathname-c))
    (unless (null-pointer-p output-pathname-c)
      (foreign-string-free output-pathname-c))))

(defcfun ("lttb_fst_processor_postgenerate_in_mem" fst-processor-postgenerate-in-mem*)
  :int
  (fst_processor :pointer)
  (input_buf :pointer)
  (output_buf :pointer)
  (output_buf_size :pointer))

(defun fst-processor-postgenerate-in-mem (fst-processor input-string)
  (with-foreign-string (input-string* input-string)
    (let ((output-buffer-pointer (cffi:foreign-alloc :pointer))
	  (size-pointer (cffi:foreign-alloc :pointer)))
      (cl-lttb::fst-processor-postgenerate-in-mem* fst-processor
						   input-string*
						   output-buffer-pointer
						   size-pointer)
      (let* ((size (cffi:mem-ref size-pointer :int))
	     (output-buffer-pointer* (cffi:mem-ref output-buffer-pointer :pointer))
	     (output-buffer (cffi:foreign-string-to-lisp output-buffer-pointer* :count size)))
	(cffi:foreign-free output-buffer-pointer)
	(cffi:foreign-free size-pointer)
	output-buffer))))

;; Macro

(defmacro with-fst-processor ((processor fst-pathname processor-type) &body body)
  `(let ((,processor (fst-processor-new)))
     (unwind-protect
	  (progn
	    (fst-processor-load ,processor ,fst-pathname)
	    (ecase ,processor-type
	      (:analysis (fst-processor-init-analysis ,processor))
	      (:generator (fst-processor-init-generation ,processor))
	      (:postgenerator (fst-processor-init-postgeneration ,processor)))
	    ,@body)
       (fst-processor-destroy ,processor))))

(defmacro with-fst-generator ((generator fst-pathname) &body body)
  `(with-fst-processor (,generator ,fst-pathname :generator)
     ,@body))

(defmacro with-fst-postgenerator ((postgenerator fst-pathname) &body body)
  `(with-fst-processor (,postgenerator ,fst-pathname :postgenerator)
     ,@body))
