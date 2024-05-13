(in-package :cl-user)
(defpackage fast-websocket
  (:use :cl
        #:fast-websocket.constants
        #:fast-websocket.ws)
  (:import-from :fast-websocket.parser
                #:make-ll-parser)
  (:import-from :fast-websocket.compose
                #:compose-frame)
  (:import-from :fast-websocket.payload
                #:fast-write-masked-sequence
                #:mask-message)
  (:import-from :fast-websocket.error
                #:websocket-error
                #:websocket-parse-error
                #:protocol-error
                #:too-large
                #:unacceptable
                #:encoding-error

                #:acceptable-error-code-p
                #:error-code)
  (:import-from :fast-io
                #:make-output-buffer
                #:finish-output-buffer
                #:fast-write-sequence)
  (:import-from :babel
                #:octets-to-string
                #:character-decoding-error)
  (:export #:make-parser
           #:compose-frame
           #:ws
           #:make-ws
           #:ws-fin
           #:ws-opcode
           #:ws-mask
           #:ws-masking-key
           #:ws-length
           #:ws-stage
           #:opcode
           #:opcode-name

           ;; errors
           #:websocket-error
           #:websocket-parse-error
           #:protocol-error
           #:too-large
           #:unacceptable
           #:encoding-error
           #:error-code))
(in-package :fast-websocket)

(defun make-payload-callback (ws message-callback ping-callback pong-callback close-callback)
  (declare (type (or null function)
                 message-callback ping-callback pong-callback close-callback))
  (let ((buffer (make-output-buffer)))
    (lambda (payload &key (start 0) (end (length payload)) partial-frame)
      (declare (optimize (speed 3) (safety 2))
               (type (simple-array (unsigned-byte 8) (*)) payload)
               (type integer start end))
      (ecase (opcode-name (ws-opcode ws))

        (:continuation
         (when (ws-mask ws)
           (mask-message payload (ws-masking-key ws) start end))
         (fast-write-sequence payload buffer start end)
         (when (and (ws-fin ws) (not partial-frame))
           (let ((message (finish-output-buffer buffer)))
             (setf buffer (make-output-buffer))
             (when message-callback
               (funcall (the function message-callback)
                        (if (eq (ws-mode ws) :text)
                            (handler-case
                                (octets-to-string message :encoding :utf-8)
                              (character-decoding-error ()
                                (error 'encoding-error)))
                            message))))))
        (:text
         (if (and (ws-fin ws) (not partial-frame))
             (when message-callback
               (handler-case
                   (funcall (the function message-callback)
                            (if (ws-mask ws)
                                (octets-to-string
                                 (let ((payload (subseq payload start end)))
                                   (mask-message payload (ws-masking-key ws)))
                                 :encoding :utf-8)
                                (octets-to-string payload
                                                  :encoding :utf-8
                                                  :start start :end end)))
                 (character-decoding-error ()
                   (error 'encoding-error))))
             (progn
               (when (ws-mask ws)
                 (mask-message payload (ws-masking-key ws) start end))
               (fast-write-sequence payload buffer start end))))
        (:binary
         (if (and (ws-fin ws) (not partial-frame))
             (when message-callback
               (funcall message-callback
                        (if (ws-mask ws)
                            (let ((payload (subseq payload start end)))
                              (mask-message payload (ws-masking-key ws)))
                            (subseq payload start end))))
             (progn
               (when (ws-mask ws)
                 (mask-message payload (ws-masking-key ws) start end))
               (fast-write-sequence payload buffer start end))))
        (:close
         (let* ((payload (subseq payload start end))
                (payload (if (ws-mask ws)
                           (mask-message payload (ws-masking-key ws))
                           payload))
                (length (- end start))
                (has-code (<= 2 length))
                (code (if has-code
                          (+ (* 256 (aref payload 0)) (aref payload 1))
                          nil)))
           (declare (type integer length))
           (unless (or (zerop length)
                       (acceptable-error-code-p code))
             (setq code (error-code :protocol-error)))

           (if has-code
             (let ((reason (octets-to-string payload :encoding :utf-8 :start 2)))
               (funcall close-callback reason :code code))
             (funcall close-callback "" :code code))))
        (:ping
         (when ping-callback
           (let ((payload (subseq payload start end)))
             (when (ws-mask ws)
               (mask-message payload (ws-masking-key ws)))
             (funcall (the function ping-callback) payload))))
        (:pong
         (when pong-callback
           (let ((payload (subseq payload start end)))
             (when (ws-mask ws)
               (mask-message payload (ws-masking-key ws)))
             (funcall (the function pong-callback) payload))))))))

(defun make-parser (ws &key
                         (require-masking t)
                         (max-length #x3ffffff)
                         message-callback  ;; (message)
                         ping-callback     ;; (payload)
                         pong-callback     ;; (payload)
                         close-callback    ;; (payload &key code)
                         error-callback)   ;; (code reason)
  (declare (type (or null function) error-callback))
  (let ((parser
          (make-ll-parser ws
                          :require-masking require-masking
                          :max-length max-length
                          :payload-callback
                          (make-payload-callback ws
                                                 message-callback
                                                 ping-callback
                                                 pong-callback
                                                 close-callback)))
        (bufferedp nil)
        (buffer (make-output-buffer)))
    (lambda (data &key start end)
      (setq start (or start 0)
            end   (or end (length data)))

      (when bufferedp
        (fast-write-sequence data buffer start end)
        (setq data (finish-output-buffer buffer))
        (setq buffer (make-output-buffer)
              bufferedp nil)
        (setq start 0
              end (length data)))
      (multiple-value-bind (i eofp)
          (handler-case
              (funcall parser data :start start :end (or end (length data)))
            (protocol-error (e)
              (when error-callback
                (funcall (the function error-callback)
                         (error-code e)
                         (princ-to-string e)))))
        (when eofp
          (setq bufferedp t)
          (fast-write-sequence data buffer i))))))
