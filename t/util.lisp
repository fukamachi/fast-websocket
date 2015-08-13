(in-package :cl-user)
(defpackage fast-websocket-test.util
  (:use :cl)
  (:export :bv))
(in-package :fast-websocket-test.util)

(defun bv (&rest args)
  (make-array (length args) :element-type '(unsigned-byte 8) :initial-contents args))
