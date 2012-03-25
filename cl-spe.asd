;;;; cl-spe.asd

(asdf:defsystem #:cl-spe
  :name "cl-spe"
  :description "Read WinView/WinSpec SPE files."
  :license "Public Domain"
  :author "Sumant Oemrawsingh"
  :depends-on (#:alexandria #:ieee-floats)
  :components ((:file "package")
	       (:file "read"
		      :depends-on ("package"))
	       (:file "write"
		      :depends-on ("package"))
               (:file "cl-spe"
		      :depends-on ("package" "read" "write"))))
