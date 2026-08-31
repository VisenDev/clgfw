

(eval-when (:compile-toplevel)
  
  (defclass field-metadata ()
    (name type size category accessor offset))
  
  (defun symbolicate (&rest things)
    (intern (string-upcase
             (apply #'concatenate 'string
                    (mapcar (lambda (thing) (format nil "~a" thing)) things)))))

  (defun float->fixed-point (float field-metadata)
    (with-slots (name type size category accessor offset) field-metadata
      
      )
    )

  (defun fixed-point->float (fixed-point field-metadata)
    (with-slots (name type size category accessor offset) field-metadata
      
      )
    )

  (defun value->uint (value field-metadata)
    (with-slots (name type size category accessor offset) field-metadata
      (ecase category
        (:bool `(if ,value 1 0))
        (:char `(char-code ,value))
        (:uint value)
        (:int  `(ldb (byte ,size ,offset) ,value)) ;; converts int to uint
        (:fixed-point (float->fixed-point value field-metadata)))))

  (defun uint->int (uint size)
    `(if (logbitp (1- ,uint) ,uint)
        (- ,uint (ash 1 ,size))
        ,uint))

  (defun uint->value (uint field-metadata)
    (with-slots (name type size category accessor offset) field-metadata
      (ecase category
        (:bool `(if (= ,uint 0) nil t))
        (:char `(code-char ,uint))
        (:uint value)
        (:int (uint->int uint size))
        (:fixed-point (float->fixed-point uint field-metadata))))
    )
  
  (defun fields->metadata (record-name fields)
    (loop
      :with total-offset = 0
      :for (fname ftype) :in fields
      :for i :from 0
      :for current-offset = total-offset
      :collect
      (let ((meta (make-instance 'field-metadata)))
        (with-slots (name type size category accessor offset) meta
          (setf name fname)
          (setf type ftype)
          (setf offset current-offset)
          (setf accessor (symbolicate record-name '- fname))
          (ecase ftype
            (bool (setf size 8 category :bool))
            (char (setf size 8 category :char))
            (u8  (setf size  8 category :uint))
            (u16 (setf size 16 category :uint))
            (u32 (setf size 32 category :uint))
            (u64 (setf size 64 category :uint))

            (i8  (setf size  8 category :int))
            (i16 (setf size 16 category :int))
            (i32 (setf size 32 category :int))
            (i64 (setf size 64 category :int))
            
            (f32 (setf size 32 category :fixed-point))
            (f64 (setf size 64 category :fixed-point)))
          (incf total-offset size))
        meta))))



(defmacro defrecord (name &body fields)
  "An experiment in compound value types for common lisp using bigints"
  (let* ((field-metadatas (fields->metadata name fields))
         (readers nil)
         (writers nil))
    (mapcar
     (lambda (meta)
       (with-slots (name type size category accessor offset) meta
         
         (push
          `(define-setf-expander ,accessor (object &environment env)
             (multiple-value-bind (temps vals stores store-form access-form)
                 (get-setf-expansion `(ldb (byte ,,size ,,offset) ,object) env)
               (let ((store (first stores)))
                 (values temps
                         vals
                         stores
                         `(let ((,store
                                  ,(field-value->storage-form category store)))
                            ,store-form)
                         access-form))))
          writers)
         
         (push `(defun ,(nth i field-accessors) (,name)
                  (let ((value (ldb (byte ,size ,offset ) ,name)))
                    ,(ecase category
                       (:bool '(if (= value 0) nil t))
                       (:))))
               writers)))
     field-metadatas)
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
                 
                 ))
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
