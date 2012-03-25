;;;; read.lisp

(in-package #:cl-spe)

(declaim (inline unsigned-to-signed))
(defun unsigned-to-signed (value size)
  (let ((max-signed (expt 2 (1- (* 8 size))))
        (to-subtract (expt 2 (* 8 size))))
    (if (>= value max-signed)
        (- value to-subtract)
        value)))

(defmacro read-unsigned (size stream)
  (alexandria:with-gensyms (value)
    `(let ((,value 0))
       ,@(loop for i from 0 below (* 8 size) by 8
               collect `(setf (ldb (byte 8 ,i) ,value) (read-byte ,stream)))
       ,value)))

(defmacro read-signed (size stream)
  `(unsigned-to-signed (read-unsigned ,size ,stream) ,size))



(defun read-byte% (size stream)
  (declare (ignore size))
  (read-byte stream))

(defun read-word (size stream)
  (declare (ignore size))
  (read-unsigned 2 stream))

(defun read-dword (size stream)
  (declare (ignore size))
  (read-unsigned 4 stream))

(defun read-short (size stream)
  (declare (ignore size))
  (read-signed 2 stream))

(defun read-long (size stream)
  (declare (ignore size))
  (read-signed 4 stream))

(defun read-float (size stream)
  (declare (ignore size))
  (ieee-floats:decode-float32 (read-unsigned 4 stream)))

#+nil
(defun read-fixlen-string (size stream)
  (let ((string (make-string size)))
    (dotimes (i size)
      (setf (char string i) (code-char (read-byte stream))))
    string))

;; Reads a fixed-length string up to null termination
(defun read-fixlen-string (size stream)
  (map 'string #'code-char
       (loop repeat size
	  for byte = (read-byte stream)
	  until (= byte 0)
	  collect byte)))

(defmacro read-spe-roi% (stream)
  `(list
    ,@(loop for keyword in '(:start-x :end-x :group-x
			     :start-y :end-y :group-y)
	 collect keyword
	 collect `(read-unsigned 2 ,stream))))

(defun read-spe-roi (size stream)
  (loop repeat size
     collect (read-spe-roi% stream)))

