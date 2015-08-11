(in-package :cl-user)
(defpackage fast-websocket-test.parser
  (:use :cl
        :fast-websocket.parser
        :fast-websocket.ws
        :fast-websocket.error
        :fast-websocket-test.util
        :prove))
(in-package :fast-websocket-test.parser)

(plan 6)

(defvar *masked*
  (bv #x81 #x85 #x37 #xfa #x21 #x3d #x7f #x9f #x4d #x51 #x58))

(defvar *not-masked*
  (bv #x81 #x05 #x48 #x65 #x6c #x6c #x6f))

(subtest "basic"
  (let ((ws (make-ws)))
    (is-type (make-ll-parser ws) 'function
             "Can create a low-level parser")
    (is-type (make-ll-parser ws :require-masking t) 'function
             "Can create a low-level parser with :require-masking t")
    (let ((parser (make-ll-parser ws :require-masking t)))
      (is (funcall parser (bv)) 0)
      (is (funcall parser *masked*) (length *masked*)))))

(subtest ":require-masking"
  (let* ((ws (make-ws))
         (mask-parser (make-ll-parser ws :require-masking t)))
    (is (funcall mask-parser *masked*) (length *masked*)
        "Can parse masked frame")
    (is (ws-stage ws) 0 "Parse ended")
    (is-error (funcall mask-parser *not-masked*)
              'unacceptable
              "Raise UNACCEPTABLE (:require-masking t)"))

  (let* ((ws (make-ws))
         (unmask-parser (make-ll-parser ws :require-masking nil)))
    (is (funcall unmask-parser *not-masked*) (length *not-masked*)
        "Can parse unmasked frame")
    (is (ws-stage ws) 0 "Parse ended")
    (is-error (funcall unmask-parser *masked*)
              'unacceptable
              "Raise UNACCEPTABLE (:require-masking nil)")))

(subtest "Hello"
  (let* ((ws (make-ws))
         body
         (parser (make-ll-parser ws
                                 :require-masking nil
                                 :payload-callback
                                 (lambda (data &key start end)
                                   (setf body (subseq data start end))))))
    (funcall parser *not-masked*)
    (is (ws-stage ws) 0)
    (is (babel:octets-to-string body) "Hello")))

(subtest "incomplete frames"
  (let* ((ws (make-ws))
         (parser (make-ll-parser ws
                                 :require-masking nil
                                 :payload-callback
                                 (lambda (data &key start end)))))
    (funcall parser (subseq *not-masked* 0 1))
    (is (ws-stage ws) 1)
    (funcall parser (subseq *not-masked* 1 2))
    (is (ws-stage ws) 4))

  (let* ((ws (make-ws))
         (parser (make-ll-parser ws
                                 :require-masking t)))
    ;; first byte
    (funcall parser (subseq *masked* 0 1))
    (is (ws-stage ws) 1)
    ;; second byte
    (funcall parser (subseq *masked* 1 2))
    (is (ws-stage ws) 3)
    ;; masking-key
    (is (funcall parser (subseq *masked* 2 3)) 0
        "EOF")
    (is (ws-stage ws) 3)
    (funcall parser (subseq *masked* 2 6))
    (is (ws-stage ws) 4)
    (is (ws-masking-key ws) #(55 250 33 61) :test #'equalp)
    ;; payload
    (is (funcall parser (subseq *masked* 6 8)) 2)
    (is (ws-stage ws) 4)
    (is (funcall parser (subseq *masked* 8)) 3)
    (is (ws-stage ws) 0)))

(subtest "fragmented frames"
  (let* ((ws (make-ws))
         (body (make-string-output-stream))
         (parser (make-ll-parser ws
                                 :require-masking nil
                                 :payload-callback
                                 (lambda (data &key start end)
                                   (princ (babel:octets-to-string data :start start :end end) body)))))
    (funcall parser (bv #x01 #x03 #x48 #x65 #x6c))
    (is (ws-stage ws) 0 "1st frame ended")
    (is (ws-fin ws) nil "not the last frame")
    (is (opcode-name (ws-opcode ws)) :text "opcode is :text")

    (funcall parser (bv #x80 #x02 #x6c #x6f))
    (is (ws-stage ws) 0 "2nd frame ended")
    (is (ws-fin ws) t "the last frame")
    (is (opcode-name (ws-opcode ws)) :continuation "opcode is :continuation")
    (is (get-output-stream-string body) "Hello" "body is \"Hello\"")))

(subtest "ping"
  (let* ((ws (make-ws))
         body
         (parser (make-ll-parser ws
                                 :require-masking nil
                                 :payload-callback
                                 (lambda (data &key start end)
                                   (setf body (subseq data start end))))))
    (funcall parser (bv #x89 #x05 #x48 #x65 #x6c #x6c #x6f))
    (is (ws-stage ws) 0 "frame ended")
    (is (ws-fin ws) t)
    (is (opcode-name (ws-opcode ws)) :ping)
    (is (babel:octets-to-string body) "Hello")))

(finalize)
