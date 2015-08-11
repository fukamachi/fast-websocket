(in-package :cl-user)
(defpackage fast-websocket-test
  (:use :cl
        :fast-websocket
        :fast-websocket-test.util
        :prove))
(in-package :fast-websocket-test)

(plan 3)

(defvar *frame*
  (bv #x81 #x05 #x48 #x65 #x6c #x6c #x6f))

(let ((ws (make-ws)))
  (is-type (make-parser ws) 'function
           "Can create a parser"))

(subtest ":text frame"
  (let* ((ws (make-ws))
         (body (make-string-output-stream))
         (parser (make-parser ws
                              :require-masking nil
                              :message-callback
                              (lambda (message)
                                (princ message body)))))
    (funcall parser *frame*)
    (is (ws-stage ws) 0 "frame ended")
    (is (get-output-stream-string body) "Hello")))

(subtest "fragmented :text frames"
  (let* ((ws (make-ws))
         (body (make-string-output-stream))
         (parser (make-parser ws
                              :require-masking nil
                              :message-callback
                              (lambda (message)
                                (princ message body)))))
    (funcall parser (bv #x01 #x03 #x48 #x65 #x6c))
    (is (ws-stage ws) 0 "1st frame ended")
    (funcall parser (bv #x80 #x02 #x6c #x6f))
    (is (ws-stage ws) 0 "2nd frame ended")
    (is (get-output-stream-string body) "Hello")))

(finalize)
