(in-package :cl-user)
(defpackage fast-websocket.compose
  (:use :cl
        #:fast-websocket.constants)
  (:import-from :fast-websocket.payload
                #:mask-message)
  (:import-from :fast-io
                #:with-fast-output
                #:fast-write-sequence
                #:fast-write-byte)
  (:import-from :trivial-utf-8
                #:string-to-utf-8-bytes)
  (:export #:compose-frame))
(in-package :fast-websocket.compose)

(defun random-mask-keys ()
  (let ((keys (make-array 4 :element-type '(unsigned-byte 8))))
    (dotimes (i 4 keys)
      (setf (aref keys i) (random 256)))))

(defun compose-frame (data &key type code masking)
  (unless type
    (setq type (if (stringp data) :text :binary)))

  (when (stringp data)
    (setq data (string-to-utf-8-bytes data)))

  (let ((opcode (opcode type))
        (length (+ (length data) (if code 2 0)))
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

      (when code
        (setq data
              (concatenate '(vector (unsigned-byte 8))
                           (list (logand (ash code -8) +byte+)
                                 (logand code +byte+))
                           data)))

      (when masking
        (let ((mask-keys (random-mask-keys)))
          (fast-write-sequence mask-keys frame)
          (mask-message data mask-keys)))

      (fast-write-sequence data frame))))
