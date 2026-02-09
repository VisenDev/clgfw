;;; posix-shm.asd - system definitions
;;;
;;; Copyright (c) 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; This work is licensed under the BSD 3-Clause License.
;;; See LICENSE for more details.

(asdf:defsystem #:posix-shm
  :description "POSIX shared memory API"
  :author "Samuel Hunter"
  :license  "BSD 3-Clause"
  :version "0.0.7"

  :homepage "https://sr.ht/~shunter/posix-shm/"
  :source-control (:git "https://git.sr.ht/~shunter/posix-shm")
  :bug-tracker "https://todo.sr.ht/~shunter/posix-shm"
  :mailto "\~\s\h\u\n\t\e\r\/\p\u\b\l\i\c\-\i\n\b\o\x\@\l\i\s\t\s\.\s\r\.\h\t"

  :depends-on (#:posix-shm/ffi
               #:alexandria
               #:trivial-features)
  :serial t
  :components ((:file "shm"))

  :in-order-to ((asdf:test-op (asdf:test-op #:posix-shm/test))))

(asdf:defsystem #:posix-shm/ffi
  :description "POSIX shared memory FFI"
  :author "Samuel Hunter"
  :license "BSD 3-Clause"
  :version "0.0.7"

  :defsystem-depends-on (#:cffi-grovel)
  :depends-on (#:cffi)

  :pathname #P"ffi/"
  :serial t
  :components ((:file "package")
               (:cffi-grovel-file "grovel")
               (:file "ffi")
               (:cffi-wrapper-file "wrapper")))

(asdf:defsystem #:posix-shm/test
  :description "Test suite for posix-shm"
  :author "Samuel Hunter"
  :license "BSD 3-Clause"
  :version "0.0.6"

  :depends-on (#:posix-shm
               #:parachute
               #:osicat)
  :components ((:file "test"))

  :perform (asdf:test-op (op c)
             (uiop:symbol-call :parachute :test :xyz.shunter.posix-shm.test)))
