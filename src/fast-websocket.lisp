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
                #:protocol-error
                #:encoding-error
                #:valid-error-code-p
                #:error-code)
  (:import-from :fast-io
                #:make-output-buffer
                #:finish-output-buffer
                #:fast-write-sequence)
  (:import-from :trivial-utf-8
                #:utf-8-bytes-to-string
                #:utf-8-decoding-error)
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
           #:error-code))
(in-package :fast-websocket)

(defconstant +min-reserved-error+ 3000)
(defconstant +max-reserved-error+ 4999)

(defun make-payload-callback (ws message-callback ping-callback pong-callback close-callback)
  (declare (type (or null function)
                 message-callback ping-callback pong-callback close-callback))
  (let ((buffer (make-output-buffer)))
    (lambda (payload &key (start 0) (end (length payload)))
      (declare (optimize (speed 3) (safety 2))
               (type (simple-array (unsigned-byte 8) (*)) payload)
               (type integer start end))
      (ecase (opcode-name (ws-opcode ws))
        (:continuation
         (fast-write-sequence payload buffer start end)
         (when (ws-fin ws)
           (let ((message (finish-output-buffer buffer)))
             (when (ws-mask ws)
               (mask-message message (ws-masking-key ws)))
             (setf buffer (make-output-buffer))
             (when message-callback
               (funcall (the function message-callback)
                        (if (eq (ws-mode ws) :text)
                            (handler-case
                                (utf-8-bytes-to-string message)
                              (utf-8-decoding-error ()
                                (error 'encoding-error)))
                            message))))))
        (:text
         (if (ws-fin ws)
             (when message-callback
               (handler-case
                   (funcall (the function message-callback)
                            (if (ws-mask ws)
                                (utf-8-bytes-to-string
                                 (let ((payload (subseq payload start end)))
                                   (mask-message payload (ws-masking-key ws))))
                                (utf-8-bytes-to-string payload
                                                       :start start :end end)))
                 (utf-8-decoding-error ()
                   (error 'encoding-error))))
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
                (has-code (<= 2 length))
                (code (if has-code
                          (+ (* 256 (aref payload start)) (aref payload (1+ start)))
                          nil)))
           (declare (type integer length))
           (unless (or (zerop length)
                       (and code
                            (<= +min-reserved-error+ code +max-reserved-error+))
                       (valid-error-code-p code))
             (setq code (error-code :protocol-error)))

           (cond
             ((and has-code
                   (ws-mask ws))
              (let ((reason (subseq payload (+ start 2) end)))
                (mask-message reason (ws-masking-key ws))
                (funcall close-callback reason :start 0 :end (length reason) :code code)))
             (has-code
              (funcall close-callback payload :start (+ start 2) :end end :code code))
             (t (funcall close-callback (make-array 0 :element-type '(unsigned-byte 8))
                         :start 0 :end 0
                         :code code)))))
        (:ping
         (when ping-callback
           (funcall (the function ping-callback) payload :start start :end end)))
        (:pong
         (when pong-callback
           (funcall (the function pong-callback) payload :start start :end end)))))))

(defun make-parser (ws &key
                         (require-masking t)
                         (max-length #x3ffffff)
                         message-callback  ;; (message)
                         ping-callback     ;; (payload &key start end)
                         pong-callback     ;; (payload &key start end)
                         close-callback    ;; (payload &key start end code)
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
