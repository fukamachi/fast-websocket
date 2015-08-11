(in-package :cl-user)
(defpackage fast-websocket.constants
  (:use :cl)
  (:import-from :alexandria
                #:plist-hash-table
                #:hash-table-keys)
  (:export #:+byte+
           #:+fin+
           #:+rsv1+
           #:+rsv2+
           #:+rsv3+
           #:+opcode+
           #:+mask+
           #:+length+
           #:opcode-name
           #:opcode
           #:valid-opcode-p
           #:fragmented-opcode-p
           #:opening-opcode-p))
(in-package :fast-websocket.constants)

(defconstant +byte+    #b11111111)
(defconstant +fin+     #b10000000)
(defconstant +rsv1+    #b01000000)
(defconstant +rsv2+    #b00100000)
(defconstant +rsv3+    #b00010000)
(defconstant +opcode+  #b00001111)
(defconstant +mask+    #b10000000)
(defconstant +length+  #b01111111)

(defparameter *opcodes-map*
  (plist-hash-table '( 0 :continuation
                      1 :text
                      2 :binary
                      8 :close
                      9 :ping
                      10 :pong)
                    :test 'eql))

(defparameter *opcodes-name-map*
  (plist-hash-table '(:continuation  0
                      :text          1
                      :binary        2
                      :close         8
                      :ping          9
                      :pong         10)
                    :test 'eq))

(defun opcode-name (opcode)
  (gethash opcode *opcodes-map*))

(defun opcode (name)
  (gethash name *opcodes-name-map*))

(defparameter *opcode-valid-array*
  (let ((ary (make-array 11 :element-type 'fixnum :initial-element 0)))
    (dolist (code (hash-table-keys *opcodes-map*) ary)
      (setf (aref ary code) 1))))

(defun valid-opcode-p (opcode)
  (and (< opcode 11)
       (= (aref *opcode-valid-array* opcode) 1)))

(defparameter *fragmented-opcodes*
  (let ((ary (make-array 11 :element-type 'fixnum :initial-element 0)))
    (dolist (key '(0 1 2) ary)
      (setf (aref ary key) 1))))

(defun fragmented-opcode-p (opcode)
  (= (aref *fragmented-opcodes* opcode) 1))

(defparameter *opening-opcodes*
  (let ((ary (make-array 11 :element-type 'fixnum :initial-element 0)))
    (dolist (key '(1 2) ary)
      (setf (aref ary key) 1))))

(defun opening-opcode-p (opcode)
  (= (aref *opening-opcodes* opcode) 1))
