# fast-websocket

[![Build Status](https://travis-ci.org/fukamachi/fast-websocket.svg?branch=master)](https://travis-ci.org/fukamachi/fast-websocket)
[![Coverage Status](https://coveralls.io/repos/fukamachi/fast-websocket/badge.svg?branch=master)](https://coveralls.io/r/fukamachi/fast-websocket)
[![Quicklisp dist](http://quickdocs.org/badge/fast-websocket.svg)](http://quickdocs.org/fast-websocket/)

Optimized low-level WebSocket protocol parser/composer.

## Warning

This software is still BETA quality. The APIs will be likely to change.

## Usage

```common-lisp
(use-package :fast-websocket)

(let* ((ws (make-ws))
       (body (make-string-output-stream))
       (parser (make-parser ws
                            :require-masking t
                            :message-callback
                            (lambda (message)
                              (princ message body))
                            :close-callback
                            (lambda (payload &key code)
                              (format t "Client closed a connection: ~A (Code: ~D)~%" payload code)))))
  (funcall parser (make-array 11 :element-type '(unsigned-byte 8)
                              :initial-contents (list 129 133 225 106 10 29 169 15 102 113 142)))

  (princ (opcode-name (ws-opcode ws)))
  ;-> :TEXT

  (princ (get-output-stream-string body))
  ;-> Hello

  t)

(compose-frame "bye" :type :close :code (error-code :protocol-error))
;=> #(136 5 3 234 98 121 101)
```

## Installation

```
cd ~/common-lisp
git clone https://github.com/fukamachi/fast-websocket
```

```
(ql:quickload :fast-websocket)
```

## See Also

* [RFC 6455](https://tools.ietf.org/html/rfc6455)
* [websocket-driver](https://github.com/fukamachi/websocket-driver)

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the BSD 2-Clause License.
