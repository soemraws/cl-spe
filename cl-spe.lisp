;;;; cl-spe.lisp

(in-package #:cl-spe)

(defconstant +winview-id+ #x01234567)
(defconstant +roi-size+ 12)
(defconstant +max-roi+ 10)

(defconstant +frame-0-offset+ 4100)
(defconstant +roi-0-offset+ 1512)

(let ((types (list :float 4
		   :long 4
		   :short 2
		   :unsigned-short 2))
      (cltypes (list :float 'float
		     :long 'integer
		     :short 'integer
		     :unsigned-short 'integer))
      (reader (list :float #'read-float
		    :long #'read-long
		    :short #'read-short
		    :unsigned-short #'read-word
		    :byte #'read-byte%
		    :word #'read-word
		    :dword #'read-dword
		    :string #'read-fixlen-string
		    :char #'read-fixlen-string
		    :roi-info #'read-spe-roi))
      (writer (list :float #'write-float
		    :long #'write-long
		    :short #'write-short
		    :unsigned-short #'write-word
		    :byte #'write-byte%
		    :word #'write-word
		    :dword #'write-dword
		    :string #'write-fixlen-string
		    :char #'write-fixlen-string)))
  
  (defun code-data-type (code)
    (nth (* 2 code) types))
  
  (defun data-type-code (data-type)
    (/ (position data-type types) 2))
  
  (defun data-type-bytes (data-type)
    (getf types data-type))

  (defun data-type-cl-type (data-type)
    (getf cltypes data-type))

  (defun value-reader (type)
    (getf reader type))

  (defun read-type (type stream)
    (let (size)
      (if (listp type)
	  (setf size (cadr type)
		type (car type)))
      (funcall (value-reader type) size stream)))
  
  (defun value-writer (type)
    (getf reader type))

  (defun write-type (value type stream)
    (let (size)
      (if (listp type)
	  (setf size (cadr type)
		type (car type)))
      (funcall (value-writer type) size stream))))


(defparameter *field-specifications*
  '((:date (:char 10) 20)
    (:x-dimension :word 42)
    (:y-dimension :word 656)
    (:data-type :short 108)
    (:experiment-time-local (:char 6) 172)
    (:experiment-time-utc (:char 6) 179)
    (:number-of-frames :long 1446)
    (:number-of-roi :short 1510)
    (:roi (:roi-info 10) 1512)
    (:winview-id :long 2996)))


(defvar *mandatory-fields*
  '(:x-dimension :y-dimension :number-of-frames :data-type))


(defun read-spe-header-field% (field stream)
  (destructuring-bind (field-name field-type offset)
      (assoc field *field-specifications*)
    (file-position stream offset)
    (read-type field-type stream)))

(defun read-spe-header-field* (field stream)
  (let ((value (read-spe-header-field% field stream)))
    (list field
	  (if (eq field :data-type)
	      (code-data-type value)
	      value))))

(defun read-spe-header* (stream &optional fields)
  (unless (= +winview-id+ (read-spe-header-field% :winview-id stream))
    (error "Stream doesn't contain WinView SPE data."))
  (file-position stream 0)
  (cond ((listp fields)
	 (setf fields (union fields *mandatory-fields*)))
	((or (null fields) (eq fields :minimal) (eq fields :mandatory))
	 (setf fields *mandatory-fields*))
	((eq fields :all)
	 (setf fields (mapcar #'car *field-specifications*))))
  (let ((header (loop for field in fields
		   unless (eq field :winview-id)
		   nconc (read-spe-header-field* field stream))))
    (setf (getf header :frame-size)
	  (* (getf header :x-dimension)
	     (getf header :y-dimension)
	     (data-type-bytes (getf header :data-type))))
    header))

(defun read-spe-header (file &optional fields)
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (read-spe-header* stream fields)))

(defun read-spe-frame% (stream x-dim y-dim data-type)
  (let ((frame (make-array (list y-dim x-dim)
			   :element-type (data-type-cl-type data-type)))
	(read-value (value-reader data-type)))
    (loop for y from 0 below y-dim
       do (loop for x from 0 below x-dim
	     do (setf (aref frame y x) (funcall read-value nil stream))))
    frame))

(defun read-spe-frame* (stream &key frame-number header)
  "Return an array containing the frame with FRAME-NUMBER from
  STREAM. If FRAME-NUMBER is NIL, it is assumed to be 0 and thus the
  first frame will be returned."
  (unless header
    (setf header (read-spe-header* stream :minimal)))
  (unless frame-number
    (setf frame-number 0))
  (unless (< frame-number (getf header :number-of-frames))
    (error "Frame number out of bounds."))
  (file-position stream (+ +frame-0-offset+ (* frame-number (getf header :frame-size))))
  (read-spe-frame% stream
		   (getf header :x-dimension)
		   (getf header :y-dimension)
		   (getf header :data-type)))
  
(defun read-spe-frame (file &key frame-number)
  "Return an array containing the frame with FRAME-NUMBER from
  FILE. If FRAME-NUMBER is NIL, it is assumed to be 0 and thus the
  first frame will be returned."
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (read-spe-frame* stream :frame-number frame-number :header header)))

(defun read-spe-frames* (stream &key frame-numbers header)
  "Return a LIST of CL-ARRAYs containing all the frames in the STREAM."
  (unless header
    (seft header (read-spe-header* stream :minimal)))
  (file-position stream +frame-0-offset+)
  (when (or (null frame-numbers) (eq frame-numbers :all))
    (setf frame-numbers (loop for i from 0 below (getf header :number-of-frames)
			   collect i)))
  (loop for frame-number in frame-numbers
     collect (read-spe-frame* stream
			      :frame-number frame-number
			      :header header)))

(defun read-spe-frames (file &key frame-numbers)
  "Return a LIST of CL-ARRAYs containing all the frames in the FILE."
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (read-spe-frames* stream :frame-numbers frame-numbers)))
