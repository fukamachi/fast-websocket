(in-package :cl-user)
(defpackage fast-websocket-test
  (:use :cl
        :fast-websocket
        :fast-websocket.constants
        :fast-websocket.error
        :fast-websocket-test.util
        :trivial-utf-8
        :prove))
(in-package :fast-websocket-test)

(plan 4)

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

(subtest ":close frame"
  (let* ((ws (make-ws))
         got-code
         (body (make-string-output-stream))
         (parser (make-parser ws
                              :require-masking nil
                              :close-callback
                              (lambda (message &key code)
                                (setq got-code code)
                                (princ (utf-8-bytes-to-string message) body)))))
    (funcall parser (bv 136 5 3 232 98 121 101))
    (is (ws-opcode ws) (opcode :close))
    (is (get-output-stream-string body) "bye")
    (is (error-code-name got-code) :normal-closure)))

(subtest ":ping frame"
  (let* ((ws (make-ws))
         (body (make-string-output-stream))
         (parser (make-parser ws
                              :require-masking nil
                              :ping-callback
                              (lambda (payload)
                                (princ (utf-8-bytes-to-string payload) body)))))
    (funcall parser (bv 137 0))
    (is (opcode-name (ws-opcode ws)) :ping)
    (is (get-output-stream-string body) "")

    (funcall parser (bv 137 2 104 105))
    (is (opcode-name (ws-opcode ws)) :ping)
    (is (get-output-stream-string body) "hi"))

  (let* ((ws (make-ws))
         (body (make-string-output-stream))
         (parser (make-parser ws
                              :require-masking t
                              :ping-callback
                              (lambda (payload)
                                (princ (utf-8-bytes-to-string payload) body)))))
    (funcall parser (bv 137 128 230 106 10 164))
    (is (opcode-name (ws-opcode ws)) :ping)
    (is (get-output-stream-string body) "")

    (funcall parser (bv 137 130 52 60 46 27 92 85))
    (is (opcode-name (ws-opcode ws)) :ping)
    (is (get-output-stream-string body) "hi")))

(subtest ":pong frame"
  (let* ((ws (make-ws))
         (body (make-string-output-stream))
         (parser (make-parser ws
                              :require-masking nil
                              :pong-callback
                              (lambda (payload)
                                (princ (utf-8-bytes-to-string payload) body)))))
    (funcall parser (bv 138 0))
    (is (opcode-name (ws-opcode ws)) :pong)
    (is (get-output-stream-string body) "")

    (funcall parser (bv 138 2 104 105))
    (is (opcode-name (ws-opcode ws)) :pong)
    (is (get-output-stream-string body) "hi"))

  (let* ((ws (make-ws))
         (body (make-string-output-stream))
         (parser (make-parser ws
                              :require-masking t
                              :pong-callback
                              (lambda (payload)
                                (princ (utf-8-bytes-to-string payload) body)))))
    (funcall parser (bv 138 128 111 74 3 218))
    (is (opcode-name (ws-opcode ws)) :pong)
    (is (get-output-stream-string body) "")

    (funcall parser (bv 138 130 149 39 57 220 253 78))
    (is (opcode-name (ws-opcode ws)) :pong)
    (is (get-output-stream-string body) "hi")))

(finalize)
