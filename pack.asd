;;;; pack.asd

(asdf:defsystem #:pack
  :serial t
  :depends-on (#:alexandria
	       #:ieee-floats)
  :components ((:file "package")
               (:file "pack")))

