(declaim (optimize (speed 3) (safety 0) (space 0) (debug 0)))
(setf sb-ext:*block-compile-default* t)

(deftype unsigned-integer () `(integer 0))
(deftype u1 () `(unsigned-byte 1))
(deftype u8 () `(unsigned-byte 8))
(deftype u16 () `(unsigned-byte 16))
(deftype u32 () `(unsigned-byte 32))
(deftype u64 () `(unsigned-byte 64))

(eval-when (:compile-toplevel)
  
  (defclass field-metadata ()
    (name type size category accessor offset))
  
  (defun symbolicate (&rest things)
    (intern (string-upcase
             (apply #'concatenate 'string
                    (mapcar (lambda (thing) (format nil "~a" thing)) things)))))

  (defparameter *typedata*
    '((bool 1 :boolean)
      (char 8 :character)
      (u8   8 :unsigned-integer)
      (u16 16 :unsigned-integer)
      (u32 32 :unsigned-integer)
      (u64 64 :unsigned-integer)
      (i8  8  :integer)
      (i16 16 :integer)
      (i32 32 :integer)
      (i64 64 :integer)
      (f32 32 :real)
      (f64 64 :real)))

  (defun type->size (type)
    (second (find type *typedata* :key #'first)))
  
  (defun type->category (type)
    (third (find type *typedata* :key #'first)))
  
  (defun fields->metadata (record-name fields)
    (loop
      :with total-offset = 0
      :for (ftype fname) :in fields
      :for i :from 0
      :for current-offset = total-offset
      :collect
      (let ((meta (make-instance 'field-metadata)))
        (with-slots (name type size category accessor offset) meta
          (setf name fname)
          (setf type ftype)

          (setf offset current-offset)
          (setf accessor (symbolicate record-name '- fname))
          (setf category (type->category ftype))
          (setf size (type->size ftype))
          (incf total-offset size))
        meta)))

  (defun type->value-to-uint-function-name (type)
    (let* ((category (type->category type))
           (value-name (intern (symbol-name category)))
           (size (type->size type))
           (function-name (symbolicate value-name '-> 'u size)))
      function-name))
  
  (defun type->uint-to-value-function-name (type)
    (let* ((category (type->category type))
           (value-name (intern (symbol-name category)))
           (size (type->size type))
           (function-name (symbolicate 'u size '-> value-name)))
      function-name))

  (defun generate-value-to-uint-function (type)
    (let* ((category (type->category type))
           (value-name (intern (symbol-name category)))
           (size (type->size type))
           (function-name (type->value-to-uint-function-name type))
           (result-type (symbolicate 'u size)))
      `(progn
         (declaim (ftype (function (,value-name) ,result-type)
                         ,function-name)
                  (inline ,function-name))
         (defun ,function-name (,value-name)
           (declare (type ,value-name ,value-name))
           (the
            ,result-type
            ,(ecase category
               (:boolean `(if ,value-name 1 0))
               (:character `(char-code ,value-name))
               (:unsigned-integer `(ldb (byte ,size 0) ,value-name))
               (:integer  `(ldb (byte ,size 0) ,value-name)) ;; converts int to uint
               (:real `(round (* ,value-name ,(ash 1 (/ size 2)))))))))))

  (defun generate-uint-to-value-function (type)
    (let* ((category (type->category type))
           (value-name (intern (symbol-name category)))
           (size (type->size type))
           (function-name (type->uint-to-value-function-name type))
           (input-type (symbolicate 'u size)))
      `(progn
         (declaim (ftype (function (,input-type) ,value-name)
                         ,function-name))
         (defun ,function-name (,input-type)
           (declare (type ,input-type ,input-type))
           (the
            ,value-name
            ,(ecase category
               (:boolean `(if (= 0 ,input-type) nil t))
               (:character `(code-char ,input-type))
               (:unsigned-integer input-type)
               (:integer `(if (logbitp (1- ,size) ,input-type)
                              (- ,input-type (ash 1 ,size))
                              ,input-type))
               (:real `(/ ,input-type (ash 1 ,(/ size 2)))))))))))

(macrolet ((define-conversion-functions ()
             `(progn ,@(mapcar #'generate-value-to-uint-function
                               (mapcar #'first *typedata*))
                     ,@(mapcar #'generate-uint-to-value-function
                               (mapcar #'first *typedata*)))))
     (define-conversion-functions))


(defmacro defrecord (record-name &body |(type name)|)
  "An experiment in compound value types for common lisp using bigints"
  (let ((fields |(type name)|)
        (total-size 0)
        (readers nil)
        (writers nil)
        (set-forms nil)
        (format-forms nil)
        (make-function-name (symbolicate 'make- record-name)))
    (loop
      :with field-metadatas = (fields->metadata record-name fields)
      :for meta :in field-metadatas
      :do
         (with-slots (name type size category accessor offset) meta
           (incf total-size size)
           (push
            `(define-setf-expander ,accessor (,name &environment env)
               (multiple-value-bind (temps values stores store-forms access-forms)
                   (get-setf-expansion `(ldb (byte ,,size ,,offset) ,,name) env)
                 (values temps values stores
                          `(let ((,(first stores)
                                   (,',(type->value-to-uint-function-name type)
                                    ,(first stores))))
                             ,store-forms)
                          access-forms)))
            writers)
           
           (push `(progn
                    (declaim (ftype (function (,record-name)
                                              ,(intern (symbol-name category)))
                                    ,accessor)
                             (inline ,accessor))
                    (defun ,accessor (,record-name)
                      (let ((value (ldb (byte ,size ,offset ) ,record-name)))
                        (,(type->uint-to-value-function-name type) value)
                        )))
                 readers)

           (push `(setf (,accessor result) ,name) set-forms)

           (push `(format stream " ~a:~a" ',name
                          (,accessor ,record-name))
                 format-forms)))
    
    `(progn
       (deftype ,record-name () '(unsigned-byte ,total-size))
       ,@writers
       ,@readers
       (declaim (ftype (function ,(mapcar (lambda (type)
                                            (intern (symbol-name
                                                     (type->category type))))
                                          (mapcar #'first fields))
                                 ,record-name)
                       ,make-function-name))
       (defun ,make-function-name ,(mapcar #'second fields)
         (let ((result 0))
           ,@set-forms
           result))
       (defun ,(symbolicate 'print- record-name)
           (,record-name &optional (stream *standard-output*))
         (print-unreadable-object (,record-name stream)
           (format stream "~a" ',record-name)
           ,@(reverse format-forms))))))


(defrecord color
  (u8 r)
  (u8 g)
  (u8 b)
  (u8 a)
  (bool transparent-p))

(defrecord vec2
  (f32 x)
  (f32 y))

(make-vec2 100 100)
