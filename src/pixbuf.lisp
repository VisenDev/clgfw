(defpackage #:clgfw/pixbuf
  (:use #:cl))
(in-package #:clgfw/pixbuf)

;;; PIXBUF
(deftype pixbuf (width height) `(simple-array color (,height ,width)))

(defun pixbuf-set-pixel (pixbuf x y color)
  (setf (aref pixbuf y x)
        (color-blend (aref pixbuf y x)
                     color)))

(defun create-pixbuf (width height)
  (make-array (list height width) :element-type 'color
              :initial-element (make-color 0 0 0 0)))

(defun pixbuf-width (pixbuf)
  (second (array-dimensions pixbuf)))

(defun pixbuf-height (pixbuf)
  (first (array-dimensions pixbuf)))

(defun pixbuf-get (pixbuf x y)
  (aref pixbuf y x))

(defun pixbuf-draw-rectangle (pixbuf x y w h color)
  (loop
    :for dy :from (max y 0) :below (min (pixbuf-height pixbuf)
                                        (+ y h))
    :do
       (loop
         :for dx :from (max x 0) :below (min (pixbuf-width pixbuf)
                                             (+ x h))
         :do (pixbuf-set-pixel pixbuf x y color))))

(defun pixbuf-draw-pixbuf (&key dst dst-x dst-y src (src-x 0) (src-y 0) width height)
  (loop :for y :from )
  )
