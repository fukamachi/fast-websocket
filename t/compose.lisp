(in-package :cl-user)
(defpackage fast-websocket-test.compose
  (:use :cl
        :fast-websocket.compose
        :fast-websocket-test.util
        :trivial-utf-8
        :prove)
  (:import-from :fast-websocket
                #:error-code))
(in-package :fast-websocket-test.compose)

(plan 4)

(subtest "string"
  (is (compose-frame "hi") #(129 2 104 105) :test #'equalp)
  (is (compose-frame "hi" :type :text) #(129 2 104 105) :test #'equalp)
  (is (compose-frame "hi" :type :binary) #(130 2 104 105) :test #'equalp))

(subtest "octets"
  (is (compose-frame (string-to-utf-8-bytes "hi")) #(130 2 104 105) :test #'equalp)
  (is (compose-frame (string-to-utf-8-bytes "hi") :type :binary) #(130 2 104 105) :test #'equalp)
  (is (compose-frame (string-to-utf-8-bytes "hi") :type :text) #(129 2 104 105) :test #'equalp))

(subtest "close"
  (is (compose-frame "bye" :type :close :code (error-code :normal-closure))
      #(136 5 3 232 98 121 101)
      :test #'equalp))

(defun constant-random-mask-keys ()
  (bv 186 43 99 37))

(subtest "masking"
  (let ((original #'fast-websocket.compose::random-mask-keys))
    (setf (fdefinition 'fast-websocket.compose::random-mask-keys)
          #'constant-random-mask-keys)

    (is (compose-frame "hi" :masking t)
        #(129 130 186 43 99 37 210 66)
        :test #'equalp)

    (is (compose-frame "bye"
                       :type :close
                       :code (error-code :normal-closure)
                       :masking t)
        #(136 133 186 43 99 37 185 195 1 92 223)
        :test #'equalp)

    (setf (fdefinition 'fast-websocket.compose::random-mask-keys)
          original)))

(finalize)
