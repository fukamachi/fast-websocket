(in-package :cl-user)
(defpackage fast-websocket-test.compose
  (:use :cl
        :fast-websocket.compose
        :trivial-utf-8
        :prove)
  (:import-from :fast-websocket
                #:error-code))
(in-package :fast-websocket-test.compose)

(plan nil)

(subtest "string"
  (is (compose-frame "hi") #(129 2 104 105) :test #'equalp)
  (is (compose-frame "hi" :type :text) #(129 2 104 105) :test #'equalp)
  (is (compose-frame "hi" :type :binary) #(130 2 104 105) :test #'equalp))

(subtest "octets"
  (is (compose-frame (string-to-utf-8-bytes "hi")) #(130 2 104 105) :test #'equalp)
  (is (compose-frame (string-to-utf-8-bytes "hi") :type :binary) #(130 2 104 105) :test #'equalp)
  (is (compose-frame (string-to-utf-8-bytes "hi") :type :text) #(129 2 104 105) :test #'equalp))

(subtest "close"
  (is (compose-frame "hi" :type :close :code (error-code :normal-closure))
      #(136 4 3 232 104 105)
      :test #'equalp))

(finalize)
