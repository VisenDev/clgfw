;;; ffi/package.lisp -- FFI package definition
;;;
;;; Copyright (c) 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; This work is licensed under the BSD 3-Clause License.
;;; See LICENSE for more details.

(defpackage #:xyz.shunter.posix-shm.ffi
  (:nicknames #:posix-shm/ffi)
  (:import-from #:cl
                #:defpackage
                #:in-package)
  (:import-from #:cffi
                #:define-foreign-library
                #:use-foreign-library
                #:defcstruct
                #:defcfun)
  (:export #:blkcnt
           #:blksize
           #:dev
           #:gif
           #:ino
           #:nlink
           #:off
           #:size
           #:ssize
           #:uid
           #:c-error
           #:open-flags
           #:mode
           #:prot-flags
           #:mmap-flags
           #:whence

           #:timespec
           #:tv-sec
           #:tv-nsec

           #:stat
           #:st-dev
           #:st-ino
           #:st-mode
           #:st-nlink
           #:st-uid
           #:st-gid
           #:st-rdev
           #:st-size
           #:st-blksize
           #:st-blocks
           #:st-atim
           #:st-mtim
           #:st-ctim

           #:*errno*
           #:+map-failed+
           #:strerror
           #:shm-open
           #:ftruncate
           #:mmap
           #:munmap
           #:shm-unlink
           #:close
           #:fstat
           #:fchown
           #:fchmod))
