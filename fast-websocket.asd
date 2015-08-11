#|
  This file is a part of fast-websocket project.
  Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)
|#

#|
  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage fast-websocket-asd
  (:use :cl :asdf))
(in-package :fast-websocket-asd)

(defsystem fast-websocket
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on (:fast-io
               :trivial-utf-8
               :alexandria)
  :components ((:module "src"
                :components
                ((:file "fast-websocket" :depends-on ("ws" "parser" "payload" "error"))
                 (:file "ws")
                 (:file "parser" :depends-on ("ws" "error"))
                 (:file "payload")
                 (:file "error"))))
  :description "Optimized WebSocket protocol parser"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op fast-websocket-test))))
