(in-package :cl-user)
(defpackage fast-websocket.compose
  (:use :cl
        #:fast-websocket.constants)
  (:import-from :fast-websocket.payload
                #:mask-message)
  (:import-from :fast-websocket.error
                #:error-code
                #:acceptable-error-code-p)
  (:import-from :fast-io
                #:with-fast-output
                #:fast-write-sequence
                #:fast-write-byte)
  (:import-from :trivial-utf-8
                #:string-to-utf-8-bytes)
  (:export #:compose-frame))
(in-package :fast-websocket.compose)

(defparameter *mask-random-state*
  (make-random-state t))

(defun random-mask-keys ()
  (let ((keys (make-array 4 :element-type '(unsigned-byte 8))))
    (dotimes (i 4 keys)
      (setf (aref keys i) (random 256 *mask-random-state*)))))

(defun compose-frame (data &key start end type code masking)
  (setq start (or start 0)
        end (or end (length data)))

  (unless type
    (setq type (if (stringp data) :text :binary)))

  (when (eq type :close)
    (if code
        (unless (acceptable-error-code-p code)
          (error "Invalid error code: ~S" code))
        (setq code (error-code :normal-closure))))

  (when (stringp data)
    ;; XXX: trivial-utf-8 doesn't seem to take 'start' and 'end'. Using subseq instead.
    (setq data (string-to-utf-8-bytes (subseq data start end)))
    (setq start 0
          end (length data)))

  (let ((opcode (opcode type))
        (length (+ (- end start) (if code 2 0)))
        (masked (if masking
                    +mask+
                    0)))
    (with-fast-output (frame :vector)
      (fast-write-byte (logxor +fin+ opcode) frame)
      (cond
        ((<= length 125)
         (fast-write-byte (logxor masked length) frame))
        ((<= length 65535)
         (fast-write-byte (logxor masked 126) frame)
         (fast-write-byte (logand (ash length -8) +byte+) frame)
         (fast-write-byte (logand length +byte+) frame))
        (T
         (fast-write-byte (logxor masked 127) frame)
         (fast-write-byte (logand (ash length -56) +byte+) frame)
         (fast-write-byte (logand (ash length -48) +byte+) frame)
         (fast-write-byte (logand (ash length -40) +byte+) frame)
         (fast-write-byte (logand (ash length -32) +byte+) frame)
         (fast-write-byte (logand (ash length -24) +byte+) frame)
         (fast-write-byte (logand (ash length -16) +byte+) frame)
         (fast-write-byte (logand (ash length -8) +byte+) frame)
         (fast-write-byte (logand length +byte+) frame)))

      (if masking
          (let ((mask-keys (random-mask-keys)))
            (fast-write-sequence mask-keys frame)
            (if code
                ;; Add 'code' in front of the data
                (setq data
                      (let ((new-data (make-array length :element-type '(unsigned-byte 8))))
                        (replace new-data data
                                 :start1 2
                                 :start2 start
                                 :end2 end)
                        (setf (aref new-data 0) (logand (ash code -8) +byte+)
                              (aref new-data 1) (logand code +byte+))
                        new-data))
                ;; Call 'subseq' anyway for preventing from rewriting DATA in 'mask-message'.
                (setq data (subseq data start end)))
            (mask-message data mask-keys)
            (fast-write-sequence data frame))
          (progn
            (when code
              (fast-write-byte (logand (ash code -8) +byte+) frame)
              (fast-write-byte (logand code +byte+) frame))
            (fast-write-sequence data frame))))))
