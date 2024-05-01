(in-package :cl-user)
(defpackage fast-websocket.parser
  (:use :cl
        #:fast-websocket.constants
        #:fast-websocket.ws
        #:fast-websocket.error)
  (:import-from :alexandria
                #:named-lambda)
  (:export #:make-ll-parser))
(in-package :fast-websocket.parser)

(deftype octet () '(unsigned-byte 8))
(deftype octets () '(simple-array octet (*)))

(defun make-ll-parser (ws &key require-masking (max-length #x3ffffff) payload-callback)
  (declare (type (or null function) payload-callback)
           (type fixnum max-length))
  (named-lambda parser (data &key (start 0) (end (length data)))
    (declare (type fixnum start end)
             (type octets data)
             (optimize (speed 3) (safety 2)))
    (when (= start end)
      (return-from parser start))
    (let ((i start))
      (declare (type fixnum i))
      (tagbody
         (ecase (ws-stage ws)
           (0 (go parsing-first-byte))
           (1 (go parsing-second-byte))
           (2 (go parsing-extended-length))
           (3 (go parsing-masking-key))
           (4 (go parsing-payload)))

       parsing-first-byte
         (let* ((byte (aref data i))
                (fin (= (logand byte +fin+) +fin+)))
           (declare (type octet byte))

           (dolist (rsv (list +rsv1+ +rsv2+ +rsv3+))
             (when (= (logand byte rsv) rsv)
               (error 'protocol-error
                      :format-control "Reserved bit is on: ~A"
                      :format-arguments (list rsv))))

           (let ((opcode (logand byte +opcode+)))
             (unless (valid-opcode-p opcode)
               (error 'protocol-error
                      :format-control "Unrecognized frame opcode: ~A"
                      :format-arguments (list opcode)))

             (unless (or fin
                         (fragmented-opcode-p opcode))
               (error 'protocol-error
                      :format-control "Received fragmented control frame: opcode = ~A"
                      :format-arguments (list opcode)))

             (when (and (ws-mode ws)
                        (opening-opcode-p opcode))
               (error 'protocol-error
                      :format-control "Received new data frame but previous continuous frame is unfinished"))

             (setf (ws-fin ws) fin
                   (ws-opcode ws) opcode)))

         (incf i)
         (setf (ws-stage ws) 1)

       parsing-second-byte
         (when (= i end)
           (go end))

         (let ((byte (aref data i)))
           (declare (type octet byte))
           (incf i)
           (setf (ws-mask ws)
                 (= (logand byte +mask+) +mask+))

           (unless (eql require-masking (ws-mask ws))
             (error 'unacceptable :require-masking require-masking))

           (let ((length (logand byte +length+)))
             (setf (ws-length ws) length)
             (cond
               ((<= 0 length 125)
                (when (< max-length length)
                  (error 'too-large :length length :max-length max-length))
                (if (ws-mask ws)
                    (progn
                      (setf (ws-stage ws) 3)
                      (go parsing-masking-key))
                    (progn
                      (setf (ws-stage ws) 4)
                      (go parsing-payload))))
               (t
                (setf (ws-length-size ws) (if (= length 126) 2 8))
                (setf (ws-stage ws) 2)))))

       parsing-extended-length
         (when (< end (+ i (ws-length-size ws)))
           (return-from parser
             (values i t)))

         (let ((length 0))
           (declare (type integer length))

           (dotimes (j (ws-length-size ws))
             (setf length (+ (ash length 8) (aref data i)))
             (incf i))

           (unless (or (fragmented-opcode-p (ws-opcode ws))
                       (<= length 125))
             (error 'protocol-error
                    :format-control "Received control frame having too long payload: ~A"
                    :format-arguments (list length)))

           (when (< max-length length)
             (error 'too-large :length length :max-length max-length))

           (setf (ws-length ws) length))

         (if (ws-mask ws)
             (setf (ws-stage ws) 3)
             (progn
               (setf (ws-stage ws) 4)
               (go parsing-payload)))

       parsing-masking-key
         (when (< end (+ i 4))
           (return-from parser
             (values i t)))

         (dotimes (j 4)
           (setf (aref (ws-masking-key ws) j) (aref data i))
           (incf i))

         (setf (ws-stage ws) 4)

       parsing-payload
         (let* ((payload-end (+ i (ws-length ws)))
                (read-a-part (< end payload-end))
                (next-end (if read-a-part end payload-end)))
           (declare (type integer payload-end))
           (case (opcode-name (ws-opcode ws))
             (:continuation
              (unless (ws-mode ws)
                (error 'protocol-error
                       :format-control "Received unexpected continuation frame")))
             (:text   (unless (ws-fin ws)
                        (setf (ws-mode ws) :text)))
             (:binary (unless (ws-fin ws)
                        (setf (ws-mode ws) :binary))))

           (when payload-callback
             (funcall (the function payload-callback)
                      data
                      :start i
                      :end next-end
                      :partial-frame read-a-part))

           (if read-a-part
               (progn
                 (decf (ws-length ws) (- end i))
                 (setq i next-end))
               (progn
                 (setf (ws-stage ws) 0)

                 (setq i next-end)

                 (when (and (ws-fin ws)
                            (= (ws-opcode ws) #.(opcode :continuation)))
                   (setf (ws-mode ws) nil))

                 (unless (= i end)
                   (go parsing-first-byte)))))
       end
         (return-from parser i)))))
