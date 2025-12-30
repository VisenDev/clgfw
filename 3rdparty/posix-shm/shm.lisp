;;; shm.lisp - POSIX shared memory
;;;
;;; Copyright (c) 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; This work is licensed under the BSD 3-Clause License.
;;; See LICENSE for more details.

(defpackage #:xyz.shunter.posix-shm
  (:nicknames #:posix-shm)
  (:local-nicknames (#:a #:alexandria)
                    (#:ffi #:xyz.shunter.posix-shm.ffi))
  (:use #:cl)
  (:export #:shm
           #:shm-p
           #:open-shm
           #:open-shm-p
           #:shm-fd
           #:shm-name
           #:closed-shm
           #:stat
           #:stat-dev
           #:stat-ino
           #:stat-mode
           #:stat-nlink
           #:stat-uid
           #:stat-gid
           #:stat-rdev
           #:stat-size
           #:stat-blksize
           #:stat-blocks

           #:shm-error
           #:shm-error-name
           #:shm-does-not-exist
           #:shm-exists
           #:mmap-error

           #:open-shm
           #:open-shm*
           #:make-shm
           #:shm-stats
           #:truncate-shm
           #:chown-shm
           #:chmod-shm
           #:close-shm
           #:delete-shm
           #:mmap-shm
           #:munmap

           #:with-open-shm
           #:with-open-shm*
           #:with-mmap
           #:with-open-shm-and-mmap
           #:with-open-shm-and-mmap*))

(in-package #:xyz.shunter.posix-shm)



;; Types

(defclass shm ()
  ((%name :initarg :name :type (or string null)
          :reader shm-name)))

(defclass open-shm (shm)
  ((%fd :initarg :fd :type (integer 0)
       :reader shm-fd)))

(defclass closed-shm (shm) ())

(declaim (inline shm-p open-shm-p))
(defun shm-p (obj)
  (typep obj 'shm))

(defun open-shm-p (obj)
  (typep obj 'open-shm))

(defmethod print-object ((shm shm) stream)
  (print-unreadable-object (shm stream :type t :identity t)
    (format stream "for ~S" (slot-value shm '%name))))

;; Conditions

(define-condition shm-error (error)
  ((%name :initarg :name :type string
          :reader shm-error-name))
  (:documentation
    "A shm-error occurs during opening or closing a shared memory object,
or some low-level transaction between the OS and a preexisting shared memory object."))

(define-condition shm-does-not-exist (shm-error)
  ()
  (:report (lambda (c s)
             (format s "The shared memory object ~S does not exist"
                     (shm-error-name c)))))

(define-condition shm-exists (shm-error)
  ()
  (:report (lambda (c s)
             (format s "The shared memory object ~S exists"
                     (shm-error-name c)))))

(define-condition %simple-shm-error (shm-error)
  ((%message :initarg :message :type string))
  (:report (lambda (c s)
             (format s "~S: ~A"
                     (shm-error-name c)
                     (slot-value c '%message)))))

(define-condition mmap-error (error) ())
(define-condition %simple-mmap-error (mmap-error)
  ((%message :initarg :message :type string))
  (:report (lambda (c s)
             (princ (slot-value c '%message) s))))

(declaim (inline %raise-error %raise-mmap-error))
(defun %raise-error (&key (errno ffi:*errno*) name)
  (error '%simple-shm-error
         :message (ffi:strerror errno)
         :name name))

(defun %raise-mmap-error (&key (errno ffi:*errno*))
  (error '%simple-mmap-error
         :message (ffi:strerror errno)))

(define-condition %change-if-exists ()
  ((%value :initarg :value)))

(define-condition %change-if-dne ()
  ((%value :initarg :value)))

(defstruct stat
  (dev 0 :type fixnum)
  (ino 0 :type fixnum)
  (mode 0 :type list)
  (nlink 0 :type fixnum)
  (uid 0 :type fixnum)
  (gid 0 :type fixnum)
  (rdev 0 :type fixnum)
  (size 0 :type fixnum)
  (blksize 0 :type fixnum)
  (blocks 0 :type fixnum))



(defun make-shm (fd &optional name)
  (check-type fd (integer 0))
  (check-type name (or string null))
  (make-instance 'open-shm :fd fd :name name))

(defun delete-shm (name)
  "Deletes the shared memory object specified by NAME."
  (when (minusp (ffi:shm-unlink name))
    (let ((errno ffi:*errno*))
      (if (eq errno :enoent)
          (error 'shm-does-not-exist :name name)
          (%raise-error :errno errno :name name))))
  t)

(defun %read-new-value ()
  (format t "Enter a form to be evaluated: ")
  (multiple-value-list (eval (read))))

(defun %open-shm (name oflag mode if-exists if-does-not-exist)
  (let* ((fd (ffi:shm-open name oflag mode))
         (errno (when (minusp fd)
                  ffi:*errno*)))
    (cond
      ((>= fd 0)
       (make-instance 'open-shm :fd fd :name name))
      ;; shm_open failed because it already exists.
      ((eq errno :eexist)
       (ecase if-exists
         (:error
           (restart-case (error 'shm-exists :name name)
             (continue ()
               :report "Retry opening."
               (%open-shm name oflag mode if-exists if-does-not-exist))
             (use-value (new-name)
               :report "Try opening a different shm object."
               :interactive %read-new-value
               (%open-shm new-name oflag mode if-exists if-does-not-exist))
             (overwrite ()
               :report "Repoen with :if-exists :overwrite"
               (signal '%change-if-exists :value :overwrite))
             (truncate ()
               :report "Reopen with :if-exists :truncate"
               (signal '%change-if-exists :value :truncate))
             (supersede ()
               :report "Reopen with :if-exists :supersede"
               (signal '%change-if-exists :value :supersede))))
         ((nil) nil)
         (:supersede
           (delete-shm name)
           (%open-shm name oflag mode :error if-does-not-exist))))
      ;; shm_open failed because it doesn't exist.
      ((eq errno :enoent)
       (ecase if-does-not-exist
         (:error
           (restart-case (error 'shm-does-not-exist :name name)
             (continue ()
               :report "Retry opening."
               (%open-shm name oflag mode if-exists if-does-not-exist))
             (use-value (new-name)
               :report "Try opening a different shm object."
               :interactive %read-new-value
               (%open-shm new-name oflag mode if-exists if-does-not-exist))
             (create ()
               :report "Reopen with :if-does-not-exist :create"
               (signal '%change-if-dne :value :create))))
         ((nil) nil)))
      ;; shm_open failed for some other reason.
      (t (restart-case (%raise-error :errno errno :name name)
           (continue ()
             :report "Retry opening."
             (%open-shm name oflag mode if-exists if-does-not-exist))
           (use-value (new-name)
             :report "Try opening a different shm object."
             :interactive %read-new-value
             (%open-shm new-name oflag mode if-exists if-does-not-exist)))))))

(defun %open-options-to-oflag (direction if-exists if-does-not-exist)
  (let (flags)
    (ecase direction
      (:input (push :rdonly flags))
      (:io (push :rdwr flags)))
    (ecase if-exists
      (:overwrite)
      (:truncate (push :trunc flags))
      ((:error :supersede nil) (push :excl flags)))
    (ecase if-does-not-exist
      (:create (push :creat flags))
      ((:error nil)))
    flags))

(defun %default-dne-option (direction if-exists)
  (if (or (eq direction :input)
          (eq if-exists :overwrite)
          (eq if-exists :truncate))
      :error
      :create))

(defun open-shm (name &key (direction :input) (if-exists :overwrite)
                      (if-does-not-exist nil if-dne-provided)
                      (permissions '(:user-read :user-write
                                     :group-read :other-read)))
  (handler-case
    (let ((oflag (%open-options-to-oflag direction if-exists if-does-not-exist))
          (if-dne (if if-dne-provided
                      if-does-not-exist
                      (%default-dne-option direction if-exists))))
      (%open-shm name oflag permissions if-exists if-dne))
    (%change-if-exists (c)
      (open-shm name :direction direction
                :if-exists (slot-value c '%value)
                :if-does-not-exist if-does-not-exist
                :permissions permissions))
    (%change-if-dne (c)
      (open-shm name :direction direction
                :if-exists if-exists
                :if-does-not-exist (slot-value c '%value)
                :permissions permissions))))

(defun %random-name ()
  (loop :repeat 10
        :collect (code-char (+ #x41 (random 26)
                               (* #x20 (random 2))))
          :into suffix
        :finally (return (concatenate 'string "/xyz.shunter.posix-shm." suffix))))

(defun %open-shm* (oflag mode attempts)
  (setf oflag (list* :creat :excl oflag))
  (loop :repeat attempts
        :for name := (%random-name)
        :for shm := (%open-shm name oflag mode nil :create)
        :when shm
          :do (when (minusp (ffi:shm-unlink name))
                (let ((errno ffi:*errno*))
                  (warn "While making anonymous shm ~S, object failed to unlink with strerror ~S. This is probably an error of the posix-shm system. Continuing with fingers crossed."
                        name (ffi:strerror errno))))
              (return shm)

        :finally ;; out of attempts
          (%raise-error :name name)))

(defun open-shm* (&key (direction :input)
                       (permissions '(:user-read :user-write
                                      :group-read :other-read))
                       (attempts 100))
  (%open-shm* (ecase direction
                (:input '(:rdonly))
                (:io '(:rdwr)))
              permissions attempts))

(defun close-shm (shm)
  (etypecase shm
    (open-shm
      (when (minusp (ffi:close (shm-fd shm)))
        (%raise-error :name (shm-name shm)))
      (change-class shm 'closed-shm)
      t)
    (closed-shm
      nil)))

(defun shm-stats (shm)
  (cffi:with-foreign-object (stats '(:struct ffi:stat))
    (when (minusp (ffi:fstat (shm-fd shm) stats))
      (%raise-error :name (shm-name shm)))
    (cffi:with-foreign-slots ((ffi:st-dev ffi:st-ino ffi:st-mode ffi:st-nlink
                                      ffi:st-uid ffi:st-gid
                                      ffi:st-rdev ffi:st-size
                                      ffi:st-blksize ffi:st-blocks)
                              stats (:struct ffi:stat))
      (make-stat :dev ffi:st-dev
                 :ino ffi:st-ino
                 :mode ffi:st-mode
                 :nlink ffi:st-nlink
                 :uid ffi:st-uid
                 :gid ffi:st-gid
                 :rdev ffi:st-rdev
                 :size ffi:st-size
                 :blksize ffi:st-blksize
                 :blocks ffi:st-blocks))))

(defun truncate-shm (shm size)
  (loop :while (minusp (ffi:ftruncate (shm-fd shm) size))
        :for errno := ffi:*errno*
        :unless (eq errno :eintr)
          :do (%raise-error :errno errno :name (shm-name shm)))
  (values))

(defconstant +negative-one+
             (1- (ash 1 (* 8 (cffi:foreign-type-size 'ffi:uid))))
             "\"Negative one\", represented by a uid_t.")

(defun chown-shm (shm owner-id group-id)
  (when (minusp (ffi:fchown (shm-fd shm)
                            (or owner-id +negative-one+)
                            (or group-id +negative-one+)))
    (%raise-error :name (shm-name shm)))
  (values))

(defun chmod-shm (shm permissions)
  (when (minusp (ffi:fchmod (shm-fd shm) permissions))
    (%raise-error :name (shm-name shm)))
  (values))

(defun mmap-shm (shm length &key
                     (ptr (cffi:null-pointer))
                     (prot '(:read :write))
                     (flags '(:shared))
                     (offset 0))
  (let ((ptr (ffi:mmap ptr length (or prot (list :none))
                       (or flags '(:shared))
                       (if shm (shm-fd shm) -1)
                       offset)))
    (if (= ffi:+map-failed+ (cffi:pointer-address ptr))
        (%raise-mmap-error)
        ptr)))

(defun munmap (ptr length)
  (when (minusp (ffi:munmap ptr length))
    (%raise-mmap-error))
  (values))

(defmacro with-open-shm ((var name &rest options) &body body)
  `(let ((,var (open-shm ,name ,@options)))
     (unwind-protect
       (progn ,@body)
       (when ,var
         (close-shm ,var)))))

(defmacro with-open-shm* ((var &rest options) &body body)
  `(let ((,var (open-shm* ,@options)))
     (unwind-protect
       (progn ,@body)
       (when ,var
         (close-shm ,var)))))

(defmacro with-mmap ((var shm length &rest options) &body body)
  (a:once-only (length)
    `(let ((,var (mmap-shm ,shm ,length ,@options)))
       (unwind-protect
         (progn ,@body)
         (munmap ,var ,length)))))

(defmacro with-open-shm-and-mmap ((shm mmap shm-options
                                       (length &rest mmap-options)
                                       &key (truncate t))
                                  &body body)
  (a:once-only (length)
    `(with-open-shm (,shm ,@shm-options)
       ,(when truncate `(truncate-shm ,shm (+ ,length ,(or (getf mmap-options :offset)
                                                           0))))
       (with-mmap (,mmap ,shm ,length ,@mmap-options)
         ,@body))))

(defmacro with-open-shm-and-mmap* ((shm mmap shm-options
                                        (length &rest mmap-options)
                                 &key (truncate t))
                             &body body)
  (a:once-only (length)
    `(with-open-shm* (,shm ,@shm-options)
       ,(when truncate `(truncate-shm ,shm (+ ,length ,(or (getf mmap-options :offset)
                                                           0))))
       (with-mmap (,mmap ,shm ,length ,@mmap-options)
         ,@body))))
