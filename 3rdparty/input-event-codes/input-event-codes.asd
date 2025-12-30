;;; input-event-codes.asd - system definition
;;;
;;; Copyright (c) 2022 Samuel Hunter
;;; This software is licensed under the MIT License.
;;; See LICENSE for details.

(defsystem #:input-event-codes
  :description "Port of all constants from input-event-codes.h from both Linux and FreeBSD"
  :version "0.0.1"
  :license "MIT"
  :author "Samuel Hunter"

  :homepage "https://sr.ht/~shunter/wayflan/"
  :source-control (:git "https://git.sr.ht/~shunter/input-event-codes/")
  :bug-tracker "https://todo.sr.ht/~shunter/wayflan/"
  :mailto "\~\s\h\u\n\t\e\r\/\p\u\b\l\i\c\-\i\n\b\o\x\@\l\i\s\t\s\.\s\r\.\h\t"

  :defsystem-depends-on (#:trivial-features)
  :components ((:file "linux" :if-feature :linux)
               (:file "freebsd" :if-feature :freebsd))
  :in-order-to ((test-op (test-op #:input-event-codes/test))))

(defsystem #:input-event-codes/test
  :description "input-event-codes test suite"
  :version "0.0.1"
  :license "MIT"
  :author "Samuel Hunter"

  :depends-on (#:input-event-codes
               #:parachute)
  :components ((:file "test"))
  :perform (test-op (op c)
             (uiop:symbol-call :parachute :test
                               :xyz.shunter.input-event-codes.test)))
