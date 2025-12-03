;;; coverage.lisp - Generate coverage statistics
;;;
;;; Copyright (c) 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; This work is licensed under the BSD 3-Clause License.
;;; See LICENSE for more details.

(require :sb-cover)

(defpackage #:xyz.shunter.posix-shm.coverage
  (:use #:cl)
  (:export #:report))

(in-package #:xyz.shunter.posix-shm.coverage)



(defun report (directory)
  (declaim (optimize sb-cover:store-coverage-data))
  (asdf:oos 'asdf:load-op :posix-shm :force t)
  (asdf:test-system :posix-shm)
  (prog1
    (sb-cover:report directory)
    (declaim (optimize (sb-cover:store-coverage-data 0)))))

(report #P"/tmp/posix-shm-coverage/")
