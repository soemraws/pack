;;;; pack.asd

(asdf:defsystem #:pack
  :description "Pack and unpack binary data via Python-like struct strings."
  :author "Death"
  :license "Public Domain"
  :depends-on (#:alexandria
	       #:ieee-floats)
  :components ((:file "package")
               (:file "pack"
		      :depends-on ("package"))))

