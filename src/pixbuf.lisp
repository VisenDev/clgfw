(defpackage #:clgfw/pixbuf
  (:use #:cl)
  (:import-from #:alexandria
                #:with-gensyms)
  (:export #:pixbuf
           #:pixbuf-set
           #:create-pixbuf
           #:pixbuf-width
           #:pixbuf-height
           #:pixbuf-get
           #:pixbuf-draw-rectangle
           #:pixbuf-draw-pixbuf
           #:pixbuf-do-pixels))
(in-package #:clgfw/pixbuf)

;;; PIXBUF
(deftype pixbuf (width height) `(simple-array color (,height ,width)))

(defun pixbuf-set (pixbuf x y color)
  (setf (aref pixbuf y x)
        (clgfw:color-blend (aref pixbuf y x)
                     color)))

(defun create-pixbuf (width height)
  (make-array (list height width) :element-type 'clgfw:color
              :initial-element (clgfw:make-color 0 0 0 0)))

(defun pixbuf-width (pixbuf)
  (second (array-dimensions pixbuf)))

(defun pixbuf-height (pixbuf)
  (first (array-dimensions pixbuf)))

(defun pixbuf-get (pixbuf x y)
  (aref pixbuf y x))

(defun clamp-to-width (pixbuf x)
  (min x (1- (pixbuf-width pixbuf))))

(defun clamp-to-height (pixbuf y)
  (min y (1- (pixbuf-height pixbuf))))

(defun pixbuf-draw-rectangle (pixbuf x y w h color)
  (loop
    :for dy :from (max y 0) :below (clamp-to-height pixbuf (+ y h))
    :do (loop
          :for dx :from (max x 0) :below (clamp-to-width pixbuf (+ x w))
          :do (pixbuf-set pixbuf dx dy color))))

(defun pixbuf-draw-pixbuf (&key dst dst-x dst-y src (src-x 0) (src-y 0) width height)
  (loop :for src-dy :from src-y :below (clamp-to-height src (+ src-y height))
        :for dst-dy :from dst-y :below (clamp-to-height dst (+ dst-y height))
        :do   (loop :for src-dx :from src-x :below (clamp-to-width src (+ src-x width))
                    :for dst-dx :from dst-x :below (clamp-to-width dst (+ dst-x width))
                    :do (pixbuf-set dst dst-x dst-y
                                    (pixbuf-get src src-x src-y)))))

(defmacro pixbuf-do-pixels ((pixbuf x-varname y-varname color-varname) &body body)
  (with-gensyms (my-pixbuf width height)
    `(let ((,my-pixbuf ,pixbuf))
       (loop
         :with ,height = (pixbuf-height ,my-pixbuf)
         :with ,width = (pixbuf-width ,my-pixbuf)
         :for ,y-varname :of-type fixnum :from 0 :below ,height
         :do (loop
               :for ,x-varname :of-type fixnum :from 0 :below ,width
               :for ,color-varname = (pixbuf-get ,my-pixbuf ,x-varname ,y-varname)
               :do (let ()
                     (declare (type clgfw:color ,color-varname))
                     ,@body))))))
