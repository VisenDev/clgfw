(eval-when (:compile-toplevel)
  (defun type->size (type)
    (ecase type
      (char 1)
      (int 4)
      (short 4)
      (long 8)))
  (defun symbolicate (&rest things)
    (intern (string-upcase
             (apply #'concatenate 'string
                    (mapcar (lambda (thing) (format nil "~a" thing)) things))))))

(defmacro define-color-byte-accessor (name offset)
  `(progn
     (declaim (ftype (function (color) fixnum) ,name))
     (defun ,name (color)
       (declare (type color color)
                (optimize (speed 3) (safety 3) (debug 3)))
       (the fixnum (ldb (byte 8 ,offset) color)))
     (define-setf-expander ,name (color &environment env)
       (get-setf-expansion `(ldb (byte 8 ,,offset) ,color) env))))

(defmacro defrecord (name &body fields)
  "An experiment in compound value types for common lisp using bigints"
  (let ((total-size (loop :for (type name) :in fields
                          :sum (type->size type))))
    `(progn
       (deftype ,name () '(unsigned-byte ,total-size))
       ,@(loop :with total-offset = 0
               :for (type fname) :in fields
               :for offset = total-offset
               :for bits = (* 8 (type->size type))
               :for _ = (incf total-offset bits)
               :appending
               `((defun ,(symbolicate name '- fname) (,name)
                      (ldb (byte ,bits ,offset) ,name))
                  (define-setf-expander ,(symbolicate name '- fname)
                      (,name &environment env)
                    (get-setf-expansion `(ldb (byte ,,bits ,,offset) ,,name) env))))
       (defun ,(symbolicate 'make- name) ,(mapcar #'second fields)
              (let ((result 0))
                ,@(loop :for (type fname) :in fields
                         :for accessor = (symbolicate name '- fname)
                         :collect `(setf (,accessor result) ,fname))
                result))
       (defun ,(symbolicate 'print- name) (,name &optional (stream *standard-output*))
         (print-unreadable-object (,name stream)
           (format stream "~a" ',name)
           ,@(loop :for (type fname) :in fields
                   :for accessor = (symbolicate name '- fname)
                   :collect `(format stream " ~a:~a" ',fname
                                     (,accessor ,name))))))))

(defrecord color
  (char r)
  (char g)
  (char b)
  (char a))
