(in-package :cl-user)
(defpackage fast-websocket-test.payload
  (:use :cl
        :fast-websocket.payload
        :fast-websocket-test.util
        :prove))
(in-package :fast-websocket-test.payload)

(plan 2)

(defvar *mask-keys*
  (bv 92 246 238 121))

(is (mask-message (babel:string-to-octets "Hello")
                  *mask-keys*)
    #(20 147 130 21 51)
    :test #'equalp
    "mask-message")

(is (fast-io:with-fast-output (buffer)
      (fast-write-masked-sequence (babel:string-to-octets "Hello") buffer *mask-keys*))
    #(20 147 130 21 51)
    :test #'equalp
    "fast-write-masked-sequence")

(finalize)
