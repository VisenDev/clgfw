;;; ffi/ffi.lisp - shm foreign function interface
;;;
;;; Copyright (c) 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; This work is licensed under the BSD 3-Clause License.
;;; See LICENSE for more details.

(in-package #:xyz.shunter.posix-shm.ffi)



#-darwin
(define-foreign-library libc
  (:unix (:or "libc.so.6" "libc.so"))
  (:default "libc"))

#-(or openbsd darwin)
(define-foreign-library librt
  (:unix (:or "librt.so.1" "librt.so"))
  (:default "librt"))

#-darwin
(use-foreign-library libc)
#-(or openbsd darwin)
(use-foreign-library librt)

(defcfun "strerror" :string
  (errno c-error))

(defcfun "shm_open" :int
  (name :string)
  (oflag open-flags)
  (mode mode))

(defcfun ("ftruncate" ftruncate) :int
  (fd :int)
  (length off))

(defcfun "mmap" :pointer
  (addr :pointer)
  (length :size)
  (prot prot-flags)
  (flags mmap-flags)
  (fd :int)
  (offset off))

(defcfun "munmap" :int
  (addr :pointer)
  (length :size))

(defcfun "shm_unlink" :int
  (name :string))

(defcfun ("close" close) :int
  (fd :int))

(defcfun "fchown" :int
  (fd :int)
  (owner uid)
  (group gid))

(defcfun "fchmod" :int
  (fd :int)
  (mode mode))
