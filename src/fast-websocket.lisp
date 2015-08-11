(in-package :cl-user)
(defpackage fast-websocket
  (:use :cl
        #:fast-websocket.ws)
  (:import-from :fast-websocket.parser
                #:make-ll-parser
                #:opcode-name)
  (:import-from :fast-websocket.payload
                #:fast-write-masked-sequence
                #:mask-message)
  (:import-from :fast-websocket.error
                #:valid-error-code-p
                #:error-code)
  (:import-from :fast-io
                #:make-output-buffer
                #:finish-output-buffer
                #:fast-write-sequence)
  (:import-from :trivial-utf-8
                #:utf-8-bytes-to-string)
  (:export #:make-parser
           #:ws
           #:make-ws
           #:ws-fin
           #:ws-opcode
           #:ws-mask
           #:ws-masking-key
           #:ws-length
           #:ws-stage))
(in-package :fast-websocket)

(defconstant +min-reserved-error+ 3000)
(defconstant +max-reserved-error+ 4999)

;; TODO: too-long error
(defun make-payload-callback (ws message-callback ping-callback pong-callback close-callback)
  (let ((buffer (make-output-buffer)))
    (lambda (payload &key (start 0) end)
      (ecase (opcode-name (ws-opcode ws))
        (:continuation
         (fast-write-sequence payload buffer start end)
         (when (ws-fin ws)
           (let ((message (finish-output-buffer buffer)))
             (when (ws-mask ws)
               (mask-message message (ws-masking-key ws)))
             (setf buffer (make-output-buffer))
             (when message-callback
               (funcall message-callback
                        (if (eq (ws-mode ws) :text)
                            (utf-8-bytes-to-string message)
                            message))))))
        (:text
         (if (ws-fin ws)
             (when message-callback
               (funcall message-callback
                        (if (ws-mask ws)
                            (utf-8-bytes-to-string
                             (let ((payload (subseq payload start end)))
                               (mask-message payload (ws-masking-key ws))))
                            (utf-8-bytes-to-string payload
                                                   :start start :end end))))
             (fast-write-sequence payload buffer start end)))
        (:binary
         (if (ws-fin ws)
             (when message-callback
               (funcall message-callback
                        (if (ws-mask ws)
                            (let ((payload (subseq payload start end)))
                              (mask-message payload (ws-masking-key ws)))
                            (subseq payload start end))))
             (fast-write-sequence payload buffer start end)))
        (:close
         (let* ((length (- end start))
                (code (if (<= 2 length)
                          (* 256 (aref payload start) (aref payload (1+ start)))
                          nil)))
           (unless (or (zerop length)
                       (and code
                            (<= +min-reserved-error+ code +max-reserved-error+))
                       (valid-error-code-p code))
             (setq code (error-code :protocol-error)))

           (when (< length 125)
             (setq code (error-code :protocol-error)))

           (when close-callback
             (if (<= 2 length)
                 (funcall close-callback payload :start start :end (- end 2)
                                                 :code code)
                 (funcall close-callback #.(make-array 0 :element-type '(unsigned-byte 8))
                          :start 0 :end 0
                          :code code)))))
        (:ping
         (when ping-callback
           (funcall ping-callback payload :start start :end end)))
        (:pong
         (when pong-callback
           (funcall pong-callback payload :start start :end end)))))))

(defun make-parser (ws &key
                         (require-masking t)
                         message-callback  ;; (message)
                         ping-callback     ;; (payload &key start end)
                         pong-callback     ;; (payload &key start end)
                         close-callback)   ;; (payload &key start end code)
  (make-ll-parser ws :require-masking require-masking
                     :payload-callback
                     (make-payload-callback ws
                                            message-callback
                                            ping-callback
                                            pong-callback
                                            close-callback)))
