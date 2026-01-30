;;;; SPRITE.LISP
;;;; This file implements a basic pixel buffer to
;;;; be used as a backend for image rendering
;;;;
;;;; By Robert Burnett
;;;; Copyright 2026
;;;;
;;;; Licensed under Apache-2.0

(defpackage #:clgfw/sprite
  (:use #:cl #:clgfw))
(in-package #:clgfw/sprite)

(deftype pixels () '(simple-array color (* *)))

(defclass sprite ()
  ((%pixels :accessor pixels :type pixels)
   (%draw-fn :accessor draw-fn :initform nil)
   (%modifiedp :accessor modifiedp :initform nil)))

(defmethod (setf pixels) :before (pixels (sprite sprite))
  (setf (modifiedp sprite) t))

(defmethod width ((sprite sprite))
  (array-dimension (pixels sprite) 1))
(defmethod height ((sprite sprite))
  (array-dimension (pixels sprite) 0))

(defun empty-array-p (array)
  (loop :for dim :in (array-dimensions array)
        :minimize dim :into min
        :finally (return (= min 0))))

(defun all-the-same-color-p (pixels)
  ;; (declare (type pixels pixels))
  (when (empty-array-p pixels)
    (return-from all-the-same-color-p t))
  (loop :with target-color = (aref pixels 0 0)
        :for y :from 0 :below (array-dimension pixels 0)
        :always (loop :for x :from 0 :below (array-dimension pixels 1)
                      :for color = (aref pixels y x)
                      :always (= target-color color))))

(defun sub-2d-array (array start-x start-y w h)
  (let ((result (make-array (list h w))))
    (when (or (= 0 w)
              (= 0 h))
      (return-from sub-2d-array #2A()))
    (loop :for dy :from start-y :below (+ start-y h)
          :for i :from 0
          :do
             (loop :for dx :from start-x :below (+ start-x w)
                   :for j :from 0
                   :do (setf (aref result i j) (aref array dy dx))))
    (return-from sub-2d-array result)))

(defun split-2d-array-into-quads (array)
  (assert (= 2 (length (array-dimensions array))))
  ;; (assert (>= (array-dimension array 0) 2))
  ;; (assert (>= (array-dimension array 1) 2))
  
  (let* ((w (array-dimension array 1))
         (h (array-dimension array 0))
         (w/2 (floor (/ w 2)))
         (h/2 (floor (/ h 2)))
         )
    (list (sub-2d-array array 0 0 w/2 h/2)
          (sub-2d-array array w/2 0 (- w w/2) h/2)
          (sub-2d-array array 0 h/2 w/2 (- h h/2))
          (sub-2d-array array w/2 h/2 (- w w/2) (- h h/2)))))

;; (defun one-by-one-array-p (array)
;;   (equalp (list 1 1) (array-dimensions array)))

(defun factor-pixels-into-list-of-colored-rectangles (pixels)
  (when (all-the-same-color-p pixels)
    (return-from factor-pixels-into-list-of-colored-rectangles
      (list (clgfw::make-color-rect :x 0 :y 0
                                    :w (array-dimension pixels 1)
                                    :h (array-dimension pixels 0)
                                    :color (aref pixels 0 0)))))
  (loop :for sub-array :in (split-2d-array-into-quads pixels)
        :unless (empty-array-p sub-array)
        :appending (factor-pixels-into-list-of-colored-rectangles sub-array)))

(defun generate-draw-fn (ctx pixels)
  (loop :with draw = (clgfw::%get-draw-rectangle-function ctx)
        :with rects = (factor-pixels-into-list-of-colored-rectangles pixels)
        :for r :in rects
        :for x = (color-rect-x r)
        :for y = (color-rect-y r)
        :for w = (color-rect-w r)
        :for h = (color-rect-h r)
        :for color = (color-rect-color r)
        :collect `(,draw ctx (+ x ,x) (+ ,y y) (+ w ,w) (+ h ,h) ,color) :into cmds
        :finally
           (return
             (eval `(lambda (ctx x y)
                      (declare (optimize (speed 3)))
                      ,@cmds
                      )))))

(defmethod create-sprite (t width height)
  
  )
