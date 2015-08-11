(in-package :cl-user)
(defpackage fast-websocket.payload
  (:use :cl)
  (:import-from :fast-io
                #:fast-write-byte)
  (:import-from :alexandria
                #:once-only
                #:with-gensyms)
  (:export #:fast-write-masked-sequence
           #:mask-message))
(in-package :fast-websocket.payload)

(defun mask-byte (byte mask-key)
  (logxor byte mask-key))

(defvar *mask-key-indices*
  (let ((indices '(0 1 2 3)))
    (rplacd (last indices) indices)
    indices))

(defmacro with-masking ((byte data &key start end mask-keys (i (gensym "I"))) &body body)
  (once-only (data mask-keys start end)
    (with-gensyms (next-mask-index)
      `(do ((,i (or ,start 0) (1+ ,i))
            (,next-mask-index *mask-key-indices* (cdr ,next-mask-index)))
           ((= ,i (or ,end (length ,data)))
            ,data)
         (let ((,byte (mask-byte (aref ,data ,i) (aref mask-keys (car ,next-mask-index)))))
           ,@body)))))

(defun fast-write-masked-sequence (data output-buffer mask-keys &optional start end)
  (with-masking (byte data :start start :end end :mask-keys mask-keys)
    (fast-write-byte byte output-buffer)))

(defun mask-message (data mask-keys)
  (with-masking (byte data :start 0 :end (length data) :i i :mask-keys mask-keys)
    (setf (aref data i) byte)))
