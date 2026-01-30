;;;; BDF.LISP
;;;; This file implements a simple bitmap font parser
;;;; for use with clgfw
;;;;
;;;; This file is licensed under the Apache-2.0 License


(defpackage #:clgfw/bdf
  (:use #:cl)
  (:export #:load-bdf))
(in-package #:clgfw/bdf)

(defclass bounding-box ()
  ((width :accessor width :initarg :width)
   (height :accessor height :initarg :height)
   (offset-x :accessor offset-x :initarg :offset-x)
   (offset-y :accessor offset-y :initarg :offset-y)))

(defstruct bdf-char-lookup-key
  (character #\a :type character)
  (point-size 0 :type fixnum))

(defclass bdf-char ()
  ((startchar :accessor startchar :initarg :startchar)
   (encoding :accessor encoding :initarg :encoding)
   (swidth :accessor swidth :initarg :swidth)
   (dwidth :accessor dwidth :initarg :dwidth)
   (bbx :accessor bbx :initarg :bbx)
   (bitmap :accessor bitmap :initarg :bitmap)))

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
   (chars :accessor chars :initform (make-hash-table :size 256
                                                     :test 'equalp))))

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

(declaim (ftype (function (integer &optional integer) simple-bit-vector) int-to-bitvec))
(defun int-to-bitvec (int &optional (min-width 0))
  (declare (type integer int))
  (read-from-string
   (format nil (format nil "#*~~~d,'0b" min-width) int)))

(defun parse-chars (bdf fp count allow-non-ascii-characters)
  
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
        :do
        (expect fp "BITMAP")
        (let ((*read-base* 16)
              (bitmap (make-array (point-size bdf)
                                    :element-type 'simple-array
                                    :fill-pointer 0)))
          (loop :repeat bitmap-rows
                :do (vector-push
                     (int-to-bitvec (read fp) (point-size bdf))
                     bitmap))
          (expect fp "ENDCHAR")
          (when (or allow-non-ascii-characters
                  (< encoding 127))
            (setf (gethash (make-bdf-char-lookup-key :character (code-char encoding)
                                                     :point-size (point-size bdf))
                           (chars bdf))
                  (make-instance 'bdf-char
                                 :startchar (symbol-name char)
                                 :encoding encoding
                                 :swidth swidth
                                 :dwidth dwidth
                                 :bbx bbx
                                 :bitmap bitmap))))))

(defun parse (bdf fp allow-non-ascii-characters)
  (expect fp "STARTFONT")
  (skip-token fp)

  (loop
    :for token = (let ((*package* (find-package '#:clgfw/bdf)))
                     (read fp)) 
    :do
       (ecase token
         (font
          (setf (font bdf) (preserving-read fp)))
         (startproperties
          (parse-properties bdf fp (read fp))
          (expect fp "ENDPROPERTIES"))
         (size
          (setf (point-size bdf) (read fp))
          (setf (x-resolution bdf) (read fp))
          (setf (y-resulution bdf) (read fp)))
         (fontboundingbox
          (setf (width bdf) (read fp))
          (setf (height bdf) (read fp))
          (setf (x-offset bdf) (read fp))
          (setf (y-offset bdf) (read fp)))
         (comment
          (skip-token fp))
         (chars
          (parse-chars bdf fp (read fp) allow-non-ascii-characters))
         (endfont
          (return-from parse)))))

(defun load-bdf (path &optional (allow-non-ascii-characters nil))
  (let ((bdf (make-instance 'bdf)))
    (with-open-file (fp path)
      (let ((cl:*read-eval* nil))
        (parse bdf fp allow-non-ascii-characters))
      )
    (return-from load-bdf bdf)))
