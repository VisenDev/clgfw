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

(defclass image/sprite ()
  ((%pixels :accessor pixels :type pixels :initarg :pixels)
   (%draw-fn :accessor draw-fn :initform nil)
   (%modifiedp :accessor modifiedp :initform nil)))

(defmethod (setf pixels) :before (pixels (sprite image/sprite))
  (setf (modifiedp sprite) t))

(defmethod width ((sprite image/sprite))
  (array-dimension (pixels sprite) 1))
(defmethod height ((sprite image/sprite))
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

(defun factor-pixels-into-list-of-colored-rectangles (pixels &optional (offset-x 0) (offset-y 0))
  (when (all-the-same-color-p pixels)
    (return-from factor-pixels-into-list-of-colored-rectangles
      (list (clgfw::make-color-rect :x offset-x :y offset-y
                                    :w (array-dimension pixels 1)
                                    :h (array-dimension pixels 0)
                                    :color (aref pixels 0 0)))))

  (let (result)
    (destructuring-bind (top-left top-right bottom-left bottom-right) (split-2d-array-into-quads pixels)
      (unless (empty-array-p top-left)
        (setf result (append result (factor-pixels-into-list-of-colored-rectangles
                                     top-left
                                     offset-x
                                     offset-y))))
      (unless (empty-array-p top-right)
        (setf result (append result (factor-pixels-into-list-of-colored-rectangles
                                     top-right
                                     (+ offset-x (array-dimension top-left 1))
                                     offset-y))))
      (unless (empty-array-p bottom-left)
        (setf result (append result (factor-pixels-into-list-of-colored-rectangles
                                     bottom-left
                                     offset-x
                                     (+ offset-y (array-dimension top-left 0))))))
      (unless (empty-array-p bottom-right)
        (setf result (append result (factor-pixels-into-list-of-colored-rectangles
                                     bottom-right
                                     (+ offset-x (array-dimension top-left 0))
                                     (+ offset-y (array-dimension top-left 1)))))))
    result)
  ;; (loop :repeat 1
  ;;       :with (top-left top-right bottom-left bottom-right) = (split-2d-array-into-quads pixels)
  ;;       :unless (empty-array-p sub-array)
  ;;         :appending (factor-pixels-into-list-of-colored-rectangles sub-array))
  )

(defun generate-draw-fn (ctx pixels)
  (loop :with draw = (clgfw::%get-draw-rectangle-function ctx)
        :with rects = (factor-pixels-into-list-of-colored-rectangles pixels)
        :for r :in rects
        :for x = (color-rect-x r)
        :for y = (color-rect-y r)
        :for w = (color-rect-w r)
        :for h = (color-rect-h r)
        :for color = (color-rect-color r)
        :unless (color-invisible-p color)
        :collect `(funcall ,draw ctx (+ x ,x) (+ ,y y) ,w ,h ,color) :into cmds
        :finally
           (print cmds)
           (return
             (eval `(lambda (ctx x y)
                      (declare (optimize (speed 3)))
                      ,@cmds)))))

(defmethod clgfw:create-image (ctx width height)
  (make-instance 'image/sprite
                 :pixels (make-array (list height width) :element-type 'color
                                     :initial-element (clgfw:make-color 0 0 0 0))))

(defmethod clgfw:draw-image (ctx (image image/sprite) x y)
  (if (and (not (modifiedp image))
           (draw-fn image))
    (funcall (draw-fn image) ctx x y)
    (progn
      (setf (draw-fn image) (generate-draw-fn ctx (pixels image)))
      (setf (modifiedp image) nil)
      (draw-image ctx image x y))))

(defmethod get-window-width ((image image/sprite))
  (array-dimension (pixels image) 1))
(defmethod get-window-height ((image image/sprite))
  (array-dimension (pixels image) 0))


(defun %draw-rectangle/sprite (image x y width height color)
  ;; TODO handle color-a
  (loop :for dx :from x :below (min (+ x width) (get-window-width image))
        :do
           (loop :for dy :from y :below (min (+ y height) (get-window-height image))
                 :do (setf (aref (pixels image) dy dx) color))))

(defmethod clgfw:%get-draw-rectangle-function (ctx)
  #'%draw-rectangle/sprite)

(defmethod clgfw:destroy-image ((image image/sprite))
  (declare (ignore image)))


