#.(asdf:load-system "alexandria")

(defpackage #:clgfw/bdf
  (:use #:cl #:alexandria #:bit-smasher)
  (:export #:load-bdf))
(in-package #:clgfw/bdf)

(defclass bounding-box ()
  ((width :accessor width :initarg :width)
   (height :accessor height :initarg :height)
   (offset-x :accessor offset-x :initarg :offset-x)
   (offset-y :accessor offset-y :initarg :offset-y)))

(defclass bdf ()
  ((font :accessor font)
   (point-size :accessor point-size)
   (x-resolution :accessor x-resolution)
   (y-resolution :accessor y-resulution)
   (width :accessor width)
   (height :accessor height)
   (x-offset :accessor x-offset)
   (y-ofset :accessor y-offset)
   (font-bounding-box :accessor font-bounding-box)
   (properties :accessor properties)
   (chars :accessor chars)))

(defun expect (fp token)
  (let ((input (read fp)))
    (assert (string-equal token input))))
(defun skip-token (fp)
  (read fp))
(defun preserving-read (fp)
  (let ((*readtable* (copy-readtable)))
    (setf (readtable-case *readtable*) :preserve)
    (read fp)))

(defun parse-properties (bdf fp count)
  (loop :repeat count
        :for property = (read fp)
        :for value = (read fp)
        :appending (list property value) :into props
        :finally (setf (properties bdf) props)
        ))

(defclass bdf-char ()
  ((startchar :accessor startchar :initarg :startchar)
   (encoding :accessor encoding :initarg :encoding)
   (swidth :accessor swidth :initarg :swidth)
   (dwidth :accessor dwidth :initarg :dwidth)
   (bbx :accessor bbx :initarg :bbx)
   (bitmap :accessor bitmap :initarg :bitmap)))

(defun parse-chars (bdf fp count)
  
  (loop :with bitmap-rows = (point-size bdf)
        :repeat count
        :for char = (progn (expect fp "STARTCHAR")
                           (preserving-read fp))
        :for encoding = (progn (expect fp "ENCODING")
                               (preserving-read fp))
        :for swidth = (progn (expect fp "SWIDTH")
                             (list (read fp) (read fp)))
        :for dwidth = (progn (expect fp "DWIDTH")
                             (list (read fp) (read fp)))
        :for bbx = (progn (expect fp "BBX")
                          (let* ((w (read fp))
                                 (h (read fp))
                                 (x (read fp))
                                 (y (read fp)))
                            (make-instance 'bounding-box :width w
                                                         :height h
                                                         :offset-x x
                                                         :offset-y y)))
        :collect
        (progn
          (expect fp "BITMAP")
          (let ((*read-base* 16))
            (let ((bitmap (make-array (point-size bdf)
                                      :element-type 'simple-array
                                      :fill-pointer 0)))
              (loop :repeat bitmap-rows
                    :do (vector-push (bits<- (read fp)) bitmap))
              (expect fp "ENDCHAR")
              (make-instance 'bdf-char
                             :startchar char
                             :encoding encoding
                             :swidth swidth
                             :dwidth dwidth
                             :bbx bbx
                             :bitmap bitmap))
            ))))

(defun parse (bdf fp)
  (expect fp "STARTFONT")
  (skip-token fp)

  (loop
    :for token = (read fp) 
    :do
       (switch (token :test #'string-equal)
         ("FONT"
          (setf (font bdf) (preserving-read fp)))
         ("STARTPROPERTIES"
          (parse-properties bdf fp (read fp))
          (expect fp "ENDPROPERTIES"))
         ("SIZE"
          (setf (point-size bdf) (read fp))
          (setf (x-resolution bdf) (read fp))
          (setf (y-resulution bdf) (read fp)))
         ("FONTBOUNDINGBOX"
          (setf (width bdf) (read fp))
          (setf (height bdf) (read fp))
          (setf (x-offset bdf) (read fp))
          (setf (y-offset bdf) (read fp)))
         ("COMMENT"
          (skip-token fp))
         ("CHARS"
          (setf (chars bdf)
                (parse-chars bdf fp (read fp))))
         ("ENDFONT"
          (return-from parse)))))

(defun load-bdf (path)
  (let ((bdf (make-instance 'bdf)))
    (with-open-file (fp path)
      (let ((*read-eval* nil))
        (parse bdf fp))
      )
    (return-from load-bdf bdf)))
  
  
  
