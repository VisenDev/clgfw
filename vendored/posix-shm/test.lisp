;;; test.lisp - posix-shm test suite
;;;
;;; Copyright (c) 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; This work is licensed under the BSD 3-Clause License.
;;; See LICENSE for more details.

(defpackage #:xyz.shunter.posix-shm.test
  (:use #:cl #:parachute)
  (:local-nicknames (#:shm #:xyz.shunter.posix-shm)))

(in-package #:xyz.shunter.posix-shm.test)



(defparameter +shm-name+
  "/xyz.shunter.posix-shm.test")

(defparameter +int-size+
  (cffi:foreign-type-size :int))

(define-test open-shm
  (ignore-errors (shm:delete-shm +shm-name+))
  (let ((shm (shm:open-shm +shm-name+ :if-does-not-exist :create
                           :permissions '(:user-all))))
    (of-type (and shm:shm shm:open-shm) shm)
    (of-type (integer 0) (shm:shm-fd shm))
    (true (shm:close-shm shm))

    (of-type (and shm:shm shm:closed-shm) shm)
    (fail (shm:shm-fd shm))
    (finish (shm:close-shm shm)))

  (let ((shm (shm:open-shm*)))
    (of-type shm:open-shm shm)
    (true (shm:close-shm shm))
    (fail (shm:open-shm (shm:shm-name shm))
          'shm:shm-does-not-exist))

  (fail (shm:open-shm* :attempts 0)
        'shm:shm-error)

  (shm:with-open-shm (shm +shm-name+)
    (of-type shm:open-shm shm))

  (shm:with-open-shm* (shm)
    (of-type shm:open-shm shm))

  (fail (shm:close-shm (shm:make-shm 9999))
        'shm:shm-error)

  (fail (shm:open-shm +shm-name+ :permissions '(:flag-that-doesnt-exist)))

  ;; Cleanup
  (shm:delete-shm +shm-name+))

(define-test open-shm.can-use-various-options
  :parent open-shm
  (shm:with-open-shm (shm +shm-name+ :if-exists nil
                          :if-does-not-exist :create
                          :permissions '(:user-all))
    (of-type shm:open-shm shm))

  (fail (shm:open-shm +shm-name+ :if-exists :error
                      :if-does-not-exist :create)
        'shm:shm-exists)

  (shm:with-open-shm (shm +shm-name+ :if-exists :supersede
                          :if-does-not-exist :create
                          :permissions '(:user-all)
                          )
    (of-type shm:open-shm shm))

  (shm:with-open-shm (shm +shm-name+ :if-exists :truncate)
    (of-type shm:open-shm shm))

  (shm:delete-shm +shm-name+)
  (shm:with-open-shm (shm +shm-name+ :if-exists :error :if-does-not-exist :create
                          :permissions '(:user-all))
    (of-type shm:open-shm shm))
  (shm:delete-shm +shm-name+)
  (shm:with-open-shm (shm +shm-name+ :if-exists nil :if-does-not-exist :create
                          :permissions '(:user-all))
    (of-type shm:open-shm shm))

  (shm:delete-shm +shm-name+)
  (fail (shm:open-shm +shm-name+ :if-does-not-exist :error)
        'shm:shm-does-not-exist)
  (shm:with-open-shm (shm +shm-name+ :if-does-not-exist nil)
    (is eq nil shm)))

(define-test open-shm.can-use-various-permissions
  :parent open-shm
  (shm:with-open-shm* (shm :permissions '(:user-exec :user-write :user-read
                                          :group-exec :group-write :group-read
                                          :other-exec :other-write :other-read))
    (of-type shm:open-shm shm))


  (shm:with-open-shm* (shm :permissions '(:xusr :wusr :rusr
                                          :xgrp :wgrp :rgrp
                                          :xoth :woth :roth))
    (of-type shm:open-shm shm))


  (shm:with-open-shm* (shm :permissions '(:user-all :group-all :other-all))
    (of-type shm:open-shm shm))


  (shm:with-open-shm* (shm :permissions '(:rwxu :rwxg :rwxo))
    (of-type shm:open-shm shm)))

(define-test open-shm.cant-open-names-with-multiple-slashes
  :parent open-shm
  (fail (shm:open-shm "/xyz.shunter.posix-shm.test/bad/name///"
                      :if-does-not-exist :create)
        'shm:shm-error))

(define-test delete-shm
  :depends-on (open-shm)
  ;; Ensure shm with given name exists, then close it immediately
  (shm:close-shm (shm:open-shm +shm-name+ :if-does-not-exist :create))

  ;; Delete the shm
  (finish (shm:delete-shm +shm-name+))

  ;; Ensure the shm doesn't exist
  (fail (shm:open-shm +shm-name+ :if-does-not-exist :error))

  ;; Fail if delete-shm tries to operate on a shm that doesn't exist
  (fail (shm:delete-shm +shm-name+)
        'shm:shm-does-not-exist)

  ;; Fail if the name is invalid
  (fail (shm:open-shm "/xyz.shunter.posix-shm.test/bad/name///")
        '(and shm:shm-error (not shm:shm-does-not-exist))))

(define-test truncate-shm
  :depends-on (open-shm)
  (shm:with-open-shm (shm +shm-name+ :direction :io :if-does-not-exist :create)
    (finish (shm:truncate-shm shm 100)))

  (shm:with-open-shm (shm +shm-name+)
    (fail (shm:truncate-shm shm 100)
          'shm:shm-error))

  (shm:with-open-shm (shm +shm-name+ :direction :input)
    (fail (shm:truncate-shm shm 100)
          'shm:shm-error))

  (shm:with-open-shm (shm +shm-name+ :direction :io)
    (fail (shm:truncate-shm shm -1)
          'shm:shm-error))

  ;; Cleanup
  (shm:delete-shm +shm-name+))

(define-test mmap
  :depends-on (open-shm truncate-shm)

  (shm:with-open-shm* (shm :direction :io)
    (shm:truncate-shm shm 100)
    (let ((ptr (shm:mmap-shm shm 100)))
      (true (cffi:pointerp ptr))
      (of-type integer
               (cffi:mem-aref ptr :int))
      (shm:munmap ptr 100)))

  (shm:with-open-shm* (shm :direction :io)
    (shm:truncate-shm shm 100)
    (shm:with-mmap (ptr shm 100)
      (true (cffi:pointerp ptr))
      (of-type integer
               (cffi:mem-aref ptr :int))))

  (shm:with-open-shm-and-mmap (shm ptr (+shm-name+ :direction :io
                                                   :if-does-not-exist :create)
                             (100 :prot '(:read)))
    (of-type shm:open-shm shm)
    (true (cffi:pointerp ptr))
    (of-type integer
             (cffi:mem-aref ptr :int)))
  (shm:delete-shm +shm-name+)

  (shm:with-open-shm-and-mmap* (shm ptr (:direction :io)
                              (100 :prot '(:read)))
    (of-type shm:open-shm shm)
    (true (cffi:pointerp ptr))
    (of-type integer
             (cffi:mem-aref ptr :int)))

  ;; mmapping a read-only shm with read-write protections
  (shm:with-open-shm* (shm)
    (fail (shm:mmap-shm shm 10 :prot '(:read :write))
          'shm:mmap-error))

  ;; writing to and reading from a read-write shm object
  (shm:with-open-shm-and-mmap* (shm ptr (:direction :io) ((* +int-size+ 5)))
    (loop :for i :upto 5 :do (setf (cffi:mem-aref ptr :int i) (* 10 i)))
    (loop :for i :upto 5
          :do (is = (* i 10)
                  (cffi:mem-aref ptr :int i))))

  ;;; Note, the memory fault related tests are disabled
  ;;; in ecl because the memory faults seems to cause
  ;;; errors that cannot be easily continued from

  ;; Memory fault on reading with no read protection
  (shm:with-open-shm-and-mmap* (shm ptr (:direction :io)
                                (+int-size+ :prot ()))
    (skip-on '(or :ecl) "Memory fault test"
        (fail (cffi:mem-aref ptr :int))))

  ;; Memory fault on writing with no write protection
  (shm:with-open-shm-and-mmap* (shm ptr (:direction :io)
                                (+int-size+ :prot ()))
    (skip-on '(or :ecl) "Memory fault test"
        (fail (cffi:mem-aref ptr :int))))

  ;; Two ptr's mmapped from the same shm object should share values
  (shm:with-open-shm* (shm :direction :io)
    (shm:truncate-shm shm +int-size+)
    (shm:with-mmap (write-ptr shm +int-size+ :prot '(:write))
      (shm:with-mmap (read-ptr shm +int-size+ :prot '(:read))
        (loop :repeat 3
              :with the-int := (random 1000)
              :do (setf (cffi:mem-ref write-ptr :int) the-int)
              (is = the-int (cffi:mem-ref read-ptr :int))))))

  ;; Two mmaps from two shm objects from the same path should share values
  (ignore-errors (shm:delete-shm +shm-name+))
  (shm:with-open-shm-and-mmap (shm1 write-ptr
                              (+shm-name+ :direction :io
                                          :if-exists :error
                                          :if-does-not-exist :create)
                              (+int-size+ :prot '(:write)))
    (shm:with-open-shm-and-mmap (shm2 read-ptr (+shm-name+)
                                      (+int-size+ :prot '(:read))
                                      :truncate nil)
      (loop :repeat 3
            :with the-int := (random 1000)
            :do (setf (cffi:mem-ref write-ptr :int) the-int)
            (is = the-int (cffi:mem-ref read-ptr :int)))))

  (fail (shm:munmap (cffi:null-pointer) 0)
        'shm:mmap-error)

  ;; Cleanup
  (shm:delete-shm +shm-name+))

(define-test shm-stats
  :depends-on (open-shm)
  (shm:with-open-shm* (shm :permissions ())
    (let ((stat (shm:shm-stats shm)))
      (of-type shm:stat stat)
      (of-type fixnum (shm:stat-dev stat))
      (of-type fixnum (shm:stat-ino stat))
      (is eq () (shm:stat-mode stat))
      (of-type fixnum (shm:stat-nlink stat))
      (of-type fixnum (shm:stat-uid stat))
      (of-type fixnum (shm:stat-gid stat))
      (of-type fixnum (shm:stat-rdev stat))
      (is = 0 (shm:stat-size stat))
      (of-type fixnum (shm:stat-blksize stat))
      (is = 0 (shm:stat-blocks stat))))

  (fail (shm:shm-stats (shm:make-shm 99999))
        'shm:shm-error))

(define-test chown-shm
  :depends-on (open-shm shm-stats)
  (shm:with-open-shm* (shm :direction :io)
    ;; Because only privileged processes may change the owner of a file,
    ;; and the GID of shm's are usually root,
    ;; I'll restrict myself to no-ops and failures.
    (let ((the-uid (osicat-posix:getuid))
          (the-gid (shm:stat-gid (shm:shm-stats shm))))
      (finish (shm:chown-shm shm the-uid nil))
      (let ((stat (shm:shm-stats shm)))
        (is eq the-uid (shm:stat-uid stat))
        (is eq the-gid (shm:stat-gid stat)))

      (finish (shm:chown-shm shm nil nil))
      (let ((stat (shm:shm-stats shm)))
        (is eq the-uid (shm:stat-uid stat))
        (is eq the-gid (shm:stat-gid stat))))

    ;; Permissions error
    (fail (shm:chown-shm shm 0 nil)
          'shm:shm-error))

  ;; Bad fd
  (fail (shm:chown-shm (shm:make-shm 99999) nil nil)
        'shm:shm-error))

(define-test chmod-shm
  :depends-on (open-shm)
  (shm:with-open-shm* (shm :direction :io)
    (finish (shm:chmod-shm shm '(:user-all))))

  ;; Bad fd
  (fail (shm:chmod-shm (shm:make-shm 99999) ())
        'shm:shm-error))

(define-test truncate-shm
  :depends-on (open-shm shm-stats)
  (shm:with-open-shm* (shm :direction :io)
    (loop :repeat 3
          :for size := (random 1000)
          :do (finish (shm:truncate-shm shm size))
              (is = size (shm:stat-size (shm:shm-stats shm)))))

  ;; Truncating a read-only shm
  (shm:with-open-shm* (shm :direction :input)
    (fail (shm:truncate-shm shm 1234)
          'shm:shm-error))

  ;; Bad fd
  (fail (shm:truncate-shm (shm:make-shm 99999) 1234)
        'shm:shm-error))
