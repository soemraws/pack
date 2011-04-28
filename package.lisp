;;;; package.lisp

;;;; python struct hack in Common Lisp by Death
;;;; gist266945-1c8c909f966ba3c63f3795e7323eda28fa3d2234 on GitHub

(defpackage #:pack
  (:use #:cl #:alexandria #:ieee-floats)
  (:export
   #:pack
   #:pack-into
   #:unpack
   #:calc-size
   #:compile-struct-string
   #:pack*
   #:unpack*
   #:struct-stream-protocol))
