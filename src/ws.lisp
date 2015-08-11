(in-package :cl-user)
(defpackage fast-websocket.ws
  (:use :cl)
  (:export #:ws
           #:make-ws
           #:ws-fin
           #:ws-opcode
           #:ws-mask
           #:ws-masking-key
           #:ws-length
           #:ws-length-size
           #:ws-mode
           #:ws-stage))
(in-package :fast-websocket.ws)

(defstruct ws
  (fin nil :type boolean)
  (opcode -1 :type fixnum)
  (mask nil :type boolean)
  (masking-key (make-array 4 :element-type '(unsigned-byte 8))
   :type (simple-array (unsigned-byte 8) (4)))
  (length 0 :type integer)

  (length-size 0 :type fixnum)
  (mode nil :type symbol)

  (stage 0 :type fixnum))
