

(eval-when (:compile-toplevel)
  
  (defclass field-metadata ()
    (name type size category accessor offset))
  
  (defun symbolicate (&rest things)
    (intern (string-upcase
             (apply #'concatenate 'string
                    (mapcar (lambda (thing) (format nil "~a" thing)) things)))))

  (defun rational->fixed-point (rational field-metadata)
    `(round (* ,rational (ash 1 ,(/ (slot-value field-metadata 'size) 2)))))

  (defun fixed-point->rational (fixed-point field-metadata)
    `(/ ,fixed-point (ash 1 (/ ,(slot-value field-metadata 'size) 2))))

  (defun value->uint (value field-metadata)
    (with-slots (name type size category accessor offset) field-metadata
      (ecase category
        (:bool `(if ,value 1 0))
        (:char `(char-code ,value))
        (:uint value)
        (:int  `(ldb (byte ,size 0) ,value)) ;; converts int to uint
        (:fixed-point (rational->fixed-point value field-metadata)))))

  (defun uint->int (uint size)
    `(if (logbitp (1- ,uint) ,uint)
        (- ,uint (ash 1 ,size))
        ,uint))

  (defun uint->value (uint field-metadata)
    (with-slots (name type size category accessor offset) field-metadata
      (ecase category
        (:bool `(if (= ,uint 0) nil t))
        (:char `(code-char ,uint))
        (:uint uint)
        (:int (uint->int uint size))
        (:fixed-point (fixed-point->rational uint field-metadata)))))
  
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
          (ecase ftype
            (bool (setf size 1 category :bool))
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



(defmacro defrecord (record-name &body fields)
  "An experiment in compound value types for common lisp using bigints"
  (let ((total-size 0)
        (readers nil)
        (writers nil)
        (set-forms nil)
        (format-forms nil))
    (loop
      :with field-metadatas = (fields->metadata record-name fields)
      :for meta :in field-metadatas
      :do
         (with-slots (name type size category accessor offset) meta
           (incf total-size size)
           (push
            `(define-setf-expander ,accessor (object &environment env)
               (multiple-value-bind (temps vals stores setter getter)
                   (get-setf-expansion object env)
                 (values
                  temps
                  vals
                  stores
                  `(dpb (byte ,,size ,,offset) ,(first stores))
                  `(ldb (byte ,,size ,,offset) ,(first stores)))))
            writers)
           
           (push `(defun ,accessor (,record-name)
                    (let ((value (ldb (byte ,size ,offset ) ,record-name)))
                      ,(uint->value 'value meta)))
                 readers)

           (push `(setf (,accessor result) ,name) set-forms)

           (push `(format stream " ~a:~a" ',record-name
                          (,accessor ,record-name))
                 format-forms)))
    
    `(progn
       (deftype ,record-name () '(unsigned-byte ,total-size))
       ,@writers
       ,@readers
       (defun ,(symbolicate 'make- record-name) ,(mapcar #'second fields)
         (let ((result 0))
           ,@set-forms
           result))
       (defun ,(symbolicate 'print- record-name)
           (,record-name &optional (stream *standard-output*))
         (print-unreadable-object (,record-name stream)
           (format stream "~a" ',record-name)
           ,@format-forms)))))

(defrecord color
  (u8 r)
  (u8 g)
  (u8 b)
  (u8 a))

(defrecord vec2
  (f32 x)
  (f32 y))

(make-vec2 100 100)
