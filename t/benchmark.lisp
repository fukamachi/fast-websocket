(in-package :cl-user)
(defpackage fast-websocket-test.benchmark
  (:use :cl
        :fast-websocket
        :fast-websocket.parser)
  (:export :run-ll-benchmark))
(in-package :fast-websocket-test.benchmark)

(defvar *masked*
  (coerce #(#x81 #x85 #x37 #xfa #x21 #x3d #x7f #x9f #x4d #x51 #x58)
          '(simple-array (unsigned-byte 8) (*))))

(defun run-ll-benchmark ()
  (let* ((ws (make-ws))
         (parser (make-ll-parser ws
                                 :require-masking t
                                 :payload-callback (lambda (data &key start end)
                                                     (declare (ignore data start end))))))
    (time
     (dotimes (i 100000)
       (funcall parser *masked*)))))
