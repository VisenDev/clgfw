(eval-when (:compile-toplevel)
  (defun type->meta (type)
    (ecase type
      (bool '(8 :bool))
      (char '(8 :char))
      
      (u8  '( 8 :uint))
      (u16 '(16 :uint))
      (u32 '(32 :uint))
      (u64 '(64 :uint))

      (i8  '( 8 :int))
      (i16 '(16 :int))
      (i32 '(32 :int))
      (i64 '(64 :int))
      
      (f32 '(32 :fixed-point))
      (f64 '(64 :fixed-point))))
  (defun symbolicate (&rest things)
    (intern (string-upcase
             (apply #'concatenate 'string
                    (mapcar (lambda (thing) (format nil "~a" thing)) things))))))

(defmacro defrecord (name &body fields)
  "An experiment in compound value types for common lisp using bigints"
  (let* ((field-names (mapcar #'first fields))
         (field-types (mapcar #'second fields))
         (field-metas (mapcar #'type->meta field-types))
         (field-sizes (mapcar #'first field-metas))
         (field-categoties (mapcar #'second field-metas))
         (total-size-bytes (/ 8 (reduce #'+ field-sizes)))
         (field-accessors (mapcar (lambda (field-name)
                                   (symbolicate name '- field-name))
                                 field-names))
         (offset 0)
         (field-offsets (mapcar (lambda (field-size)
                                  (let ((field-begin offset))
                                    (incf offset field-size)
                                    field-begin)))))
    `(progn
       (deftype ,name () '(unsigned-byte ,total-size-bytes))
       ,@(loop :for _ :in fields
               :for i :from 0
               :appending
               
               `((defun ,(nth i field-accessors) (,name)
                   (let ((value
                           (ldb (byte ,(nth i field-sizes) ,(nth i field-sizes))
                                ,name)))
                     ,(ecase )))
                 
                 (define-setf-expander ,(symbolicate name '- fname)
                     (,name &environment env)
                   (get-setf-expansion `(ldb (byte ,,bits ,,offset) ,,name) env))))
       (defun ,(symbolicate 'make- name) ,(mapcar #'second fields)
         (let ((result 0))
           ,@(loop :for (type fname) :in fields
                   :for accessor = (symbolicate name '- fname)
                   :collect `(setf (,accessor result) ,fname))
           result))
       (defun ,(symbolicate 'print- name)
           (,name &optional (stream *standard-output*))
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
