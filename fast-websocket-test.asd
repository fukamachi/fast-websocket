#|
  This file is a part of fast-websocket project.
  Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage fast-websocket-test-asd
  (:use :cl :asdf))
(in-package :fast-websocket-test-asd)

(defsystem fast-websocket-test
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on (:fast-websocket
               :trivial-utf-8
               :fast-io
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "parser")
                 (:test-file "payload")
                 (:test-file "fast-websocket")
                 (:file "benchmark")
                 (:file "util"))))
  :description "Test system for fast-websocket"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
