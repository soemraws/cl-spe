;;;; package.lisp

(defpackage #:cl-spe
  (:use #:cl)
  (:export #:read-spe-header-field*

	   #:read-spe-header*
	   #:read-spe-header

	   #:read-spe-frame*
	   #:read-spe-frame

	   #:read-spe-frames*
	   #:read-spe-frames))


