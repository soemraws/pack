;;;; pack.lisp

;;;; Python struct hack in Common Lisp written by Death (see https://gist.github.com/266945)
;;;; Modified to use ieee-floats instead of sb-kernel to increase compatibility.

(in-package #:pack)

(defvar *offset*)
(defvar *reader*)
(defvar *writer*)

(declaim (inline write-octet))
(defun write-octet (octet)
  (funcall *writer* octet))

(declaim (inline read-octet))
(defun read-octet ()
  (funcall *reader*))

(declaim (inline offset))
(defun offset ()
  *offset*)

(declaim (inline (setf offset)))
(defun (setf offset) (new-value)
  (setf *offset* new-value))

(defmacro write-unsigned-le (value size)
  (once-only (value)
    `(progn
       ,@(loop for i from 0 below (* 8 size) by 8
               collect `(write-octet (ldb (byte 8 ,i) ,value)))
       (incf (offset) ,size))))

(defmacro read-unsigned-le (size)
  (with-gensyms (value)
    `(let ((,value 0))
       ,@(loop for i from 0 below (* 8 size) by 8
               collect `(setf (ldb (byte 8 ,i) ,value) (read-octet)))
       (incf (offset) ,size)
       ,value)))

(defmacro write-unsigned-be (value size)
  (once-only (value)
    `(progn
       ,@(loop for i from (* (1- size) 8) downto 0 by 8
               collect `(write-octet (ldb (byte 8 ,i) ,value)))
       (incf (offset) ,size))))

(defmacro read-unsigned-be (size)
  (with-gensyms (value)
    `(let ((,value 0))
       ,@(loop for i from (* (1- size) 8) downto 0 by 8
               collect `(setf (ldb (byte 8 ,i) ,value) (read-octet)))
       (incf (offset) ,size)
       ,value)))

(defun char-to-byte (char-or-byte)
  (if (characterp char-or-byte)
      (char-code char-or-byte)
      char-or-byte))

(defun byte-to-char (char-or-byte)
  (if (integerp char-or-byte)
      (code-char char-or-byte)
      char-or-byte))

(defun bool-to-byte (bool)
  (if bool 1 0))

(defun byte-to-bool (byte)
  (not (zerop byte)))

(declaim (inline signed-to-unsigned))
(defun signed-to-unsigned (value size)
  (if (minusp value)
      (+ value (expt 2 (* 8 size)))
      value))

(declaim (inline unsigned-to-signed))
(defun unsigned-to-signed (value size)
  (let ((max-signed (expt 2 (1- (* 8 size))))
        (to-subtract (expt 2 (* 8 size))))
    (if (>= value max-signed)
        (- value to-subtract)
        value)))

(defmacro write-signed-le (value size)
  `(write-unsigned-le (signed-to-unsigned ,value ,size) ,size))

(defmacro write-signed-be (value size)
  `(write-unsigned-be (signed-to-unsigned ,value ,size) ,size))

(defmacro read-signed-le (size)
  `(unsigned-to-signed (read-unsigned-le ,size) ,size))

(defmacro read-signed-be (size)
  `(unsigned-to-signed (read-unsigned-be ,size) ,size))

(declaim (inline divisible-by-p))
(defun divisible-by-p (n d)
  (zerop (mod n d)))

(declaim (inline write-pad))
(defun write-pad (size)
  (loop repeat size do (write-octet 0))
  (incf (offset) size))

(defun read-pad (size)
  (loop repeat size do (read-octet))
  (incf (offset) size)
  (values))

(defun read-fixlen-string (size)
  (let ((string (make-string size)))
    (dotimes (i size)
      (setf (char string i) (code-char (read-octet))))
    (incf (offset) size)
    string))

(defun write-fixlen-string (value size)
  (dotimes (i size)
    (write-octet
     (if (>= i (length value))
         0
         (char-code (char value i)))))
  (incf (offset) size))

(defun read-pascal-string (size)
  (let ((string (make-array (1- size) :fill-pointer 0 :element-type 'character))
        (n (1- (read-octet))))
    (incf (offset))
    (dotimes (i (1- size))
      (let ((c (read-octet)))
        (when (< i n)
          (vector-push (code-char c) string))))
    (incf (offset) (1- size))
    string))

(defun write-pascal-string (value size)
  (write-octet (1+ (length value)))
  (incf (offset))
  (write-fixlen-string value (1- size)))

(defun write-float-le (value)
  (write-unsigned-le (encode-float32 value) 4))

(defun write-float-be (value)
  (write-unsigned-be (encode-float32 value) 4))

(defun read-float-le ()
  (decode-float32 (read-unsigned-le 4)))

(defun read-float-be ()
  (decode-float32 (read-unsigned-be 4)))

(defun write-double-le (value)
  (write-signed-le (encode-float64 value) 8))

(defun write-double-be (value)
  (write-signed-be (encode-float64 value) 8))

(defun read-double-le ()
  (decode-float64 (read-unsigned-le 8)))

(defun read-double-be ()
  (decode-float64 (read-unsigned-be 8)))

(defvar *pack-args* '())

(declaim (inline pack-arg))
(defun pack-arg ()
  (if (null *pack-args*)
      (error "Not enough pack arguments.")
      (pop *pack-args*)))

(defun compile-struct-string (string)
  (with-input-from-string (in string)
    (flet ((consume () (read-char in)))
      (let ((n nil)
            (pack '())
            (unpack '())
            (c (consume))
            (size 0))
        (multiple-value-bind (byte-order size/alignment)
            (case c
              (#\@ (values :little-endian :native))
              ((#\= #\<) (values :little-endian :standard))
              ((#\! #\>) (values :big-endian :standard)))
          (if (or byte-order size/alignment)
              (setf c (consume))
              (setf byte-order :little-endian size/alignment :native))
          (loop
           (let ((d (digit-char-p c)))
             (cond (d
                    (if (null n)
                        (setf n d)
                        (setf n (+ (* n 10) d))))
                   (t (let ((n (or n 1)))
                        (macrolet ((packer (&body forms)
                                     `(push `(loop repeat ,n doing ,',@forms) pack))
                                   (unpacker (&body forms)
                                     `(push `(loop repeat ,n collecting (progn ,',@forms)) unpack))
                                   (maybe-pad (n)
                                     `(when (eq :native size/alignment)
                                        (let ((to-pad (mod size ,n)))
                                          (unless (zerop to-pad)
                                            (incf size to-pad)
                                            (push `(progn (write-pad ,to-pad)) pack)
                                            (push `(progn (read-pad ,to-pad) '()) unpack))))))
                          (ecase c
                            (#\x
                             (incf size n)
                             (push `(progn (write-pad ,n)) pack)
                             (push `(progn (read-pad ,n) '()) unpack))
                            (#\c
                             (incf size n)
                             (packer (write-unsigned (char-to-byte (pack-arg)) 1))
                             (unpacker (byte-to-char (read-unsigned 1))))
                            (#\b
                             (incf size n)
                             (packer (write-signed (pack-arg) 1))
                             (unpacker (read-signed 1)))
                            (#\B
                             (incf size n)
                             (packer (write-unsigned (pack-arg) 1))
                             (unpacker (read-unsigned 1)))
                            (#\?
                             (incf size n)
                             (packer (write-unsigned (bool-to-byte (pack-arg)) 1))
                             (unpacker (byte-to-bool (read-unsigned 1))))
                            (#\h
                             (maybe-pad 2)
                             (incf size (* 2 n))
                             (packer (write-signed (pack-arg) 2))
                             (unpacker (read-signed 2)))
                            (#\H
                             (maybe-pad 2)
                             (incf size (* 2 n))
                             (packer (write-unsigned (pack-arg) 2))
                             (unpacker (read-unsigned 2)))
                            ((#\i #\l #\P)
                             (maybe-pad 4)
                             (incf size (* 4 n))
                             (packer (write-signed (pack-arg) 4))
                             (unpacker (read-signed 4)))
                            ((#\I #\L)
                             (maybe-pad 4)
                             (incf size (* 4 n))
                             (packer (write-unsigned (pack-arg) 4))
                             (unpacker (read-unsigned 4)))
                            (#\q
                             (maybe-pad 8)
                             (incf size (* 8 n))
                             (packer (write-signed (pack-arg) 8))
                             (unpacker (read-signed 8)))
                            (#\Q
                             (maybe-pad 8)
                             (incf size (* 8 n))
                             (packer (write-unsigned (pack-arg) 8))
                             (unpacker (read-unsigned 8)))
                            (#\f
                             (maybe-pad 4)
                             (incf size (* 4 n))
                             (packer (write-float (pack-arg)))
                             (unpacker (read-float)))
                            (#\d
                             (maybe-pad 8)
                             (incf size (* 8 n))
                             (packer (write-double (pack-arg)))
                             (unpacker (read-double)))
                            (#\s
                             (incf size n)
                             (push `(write-fixlen-string (pack-arg) ,n) pack)
                             (push `(list (read-fixlen-string ,n)) unpack))
                            (#\p
                             (incf size n)
                             (push `(write-pascal-string (pack-arg) ,n) pack)
                             (push `(list (read-pascal-string ,n)) unpack)))))
                      (setf n nil))))
           (handler-case
               (setf c (consume))
             (error () (return))))
          (flet ((using-byte-order (forms)
                   (ecase byte-order
                     (:little-endian
                      `(macrolet ((write-unsigned (value size) `(write-unsigned-le ,value ,size))
                                  (write-signed (value size) `(write-signed-le ,value ,size))
                                  (read-unsigned (size) `(read-unsigned-le ,size))
                                  (read-signed (size) `(read-signed-le ,size))
                                  (write-double (value) `(write-double-le ,value))
                                  (read-double () `(read-double-le))
                                  (write-float (value) `(write-float-le ,value))
                                  (read-float () `(read-float-le)))
                         ,@forms))
                       (:big-endian
                        `(macrolet ((write-unsigned (value size) `(write-unsigned-be ,value ,size))
                                    (write-signed (value size) `(write-signed-be ,value ,size))
                                    (read-unsigned (size) `(read-unsigned-be ,size))
                                    (read-signed (size) `(read-signed-be ,size))
                                    (write-double (value) `(write-double-be ,value))
                                    (read-double () `(read-double-be))
                                    (write-float (value) `(write-float-be ,value))
                                    (read-float () `(read-float-be)))
                           ,@forms)))))
            (list :pack (compile nil `(lambda () ,(using-byte-order (nreverse pack))))
                  :unpack (compile nil `(lambda () ,(using-byte-order `((nconc ,@(nreverse unpack))))))
                  :size size)))))))

(defgeneric struct-stream-protocol (stream))

(defmethod struct-stream-protocol ((stream stream))
  (values
   (lambda () (read-byte stream))
   (lambda (octet) (write-byte octet stream))))

(defmethod struct-stream-protocol ((vector vector))
  (values
   (let ((i 0))
     (lambda ()
       (prog1 (aref vector i)
         (incf i))))
   (let ((i 0))
     (lambda (octet)
       (setf (aref vector i) octet)
       (incf i)))))

(defun pack* (compiled stream &rest args)
  (when (null stream)
    (setf stream (make-array (getf compiled :size) :element-type '(unsigned-byte 8))))
  (multiple-value-bind (reader writer)
      (struct-stream-protocol stream)
    (let ((*pack-args* args)
          (*offset* 0)
          (*reader* reader)
          (*writer* writer))
      (funcall (getf compiled :pack))))
  stream)

(defun pack (string &rest args)
  (apply #'pack* (compile-struct-string string) nil args))

(define-compiler-macro pack (&whole form string &rest args)
  (if (stringp string)
      `(pack* (load-time-value (compile-struct-string ,string)) nil ,@args)
      form))

(defun pack-into (string stream &rest args)
  (apply #'pack* (compile-struct-string string) stream args))

(define-compiler-macro pack-into (&whole form string stream &rest args)
  (if (stringp string)
      `(pack* (load-time-value (compile-struct-string ,string)) ,stream ,@args)
      form))

(defun unpack* (compiled stream)
  (multiple-value-bind (reader writer)
      (struct-stream-protocol stream)
    (let ((*offset* 0)
          (*reader* reader)
          (*writer* writer))
      (funcall (getf compiled :unpack)))))

(defun unpack (string stream)
  (unpack* (compile-struct-string string) stream))

(define-compiler-macro unpack (&whole form string stream)
  (if (stringp string)
      `(unpack* (load-time-value (compile-struct-string ,string)) ,stream)
      form))

(defun calc-size (string)
  (getf (compile-struct-string string) :size))

(define-compiler-macro calc-size (&whole form string)
  (if (stringp string)
      (getf (compile-struct-string string) :size)
      form))
