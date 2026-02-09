;;; ffi/wrapper.lisp
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; This work is licensed under the BSD 3-Clause License.
;;; See LICENSE for more details.

(include "sys/stat.h")

(in-package #:xyz.shunter.posix-shm.ffi)

(defwrapper "fstat" :int
  (fd :int)
  (statbuf (:pointer (:struct stat))))
