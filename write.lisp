;;;; write.lisp

(in-package #:cl-spe)

(declaim (inline signed-to-unsigned))
(defun signed-to-unsigned (value size)
  (if (minusp value)
      (+ value (expt 2 (* 8 size)))
      value))

(defmacro write-unsigned (value size stream)
  (alexandria:once-only (value)
    `(progn
       ,@(loop for i from 0 below (* 8 size) by 8
	    collect `(write-byte (ldb (byte 8 ,i) ,value) ,stream)))))

(defmacro write-signed (value size stream)
  `(write-unsigned (signed-to-unsigned ,value ,size) ,size ,stream))



(defun write-byte% (value size stream)
  (declare (ignore size))
  (write-byte value stream))

(defun write-word (value size stream)
  (declare (ignore size))
  (write-unsigned value 2 stream))

(defun write-dword (value size stream)
  (declare (ignore size))
  (write-unsigned value 4 stream))

(defun write-short (value size stream)
  (declare (ignore size))
  (write-signed value 2 stream))

(defun write-long (value size stream)
  (declare (ignore size))
  (write-signed value 4 stream))

(defun write-float (value size stream)
  (declare (ignore size))
  (write-unsigned (ieee-floats:encode-float32 value) 4 stream))

(defun write-fixlen-string (value size stream)
  (dotimes (i size)
    (write-byte
     (if (>= i (length value))
         0
         (char-code (char value i)))
     stream)))
