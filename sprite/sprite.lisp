;;;; SPRITE.LISP
;;;; This file implements a basic pixel buffer to
;;;; be used as a backend for sprite rendering
;;;;
;;;; By Robert Burnett
;;;; Copyright 2026
;;;;
;;;; Licensed under Apache-2.0

(defpackage #:clgfw/sprite
  (:use #:cl #:clgfw))
(in-package #:clgfw/sprite)

(deftype pixels () '(simple-array color (* *)))

(defclass sprite/software-render ()
  ((%pixels :accessor pixels :type pixels :initarg :pixels)
   (%draw-fn :accessor draw-fn :initform nil)
   (%modifiedp :accessor modifiedp :initform nil)))

(defmethod (setf pixels) :before (pixels (sprite sprite/software-render))
  (setf (modifiedp sprite) t))

(defmethod width ((sprite sprite/software-render))
  (array-dimension (pixels sprite) 1))
(defmethod height ((sprite sprite/software-render))
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

(defstruct quad top-left top-right bottom-left bottom-right)

(defmacro quad-destructure (quad &body body)
  "Bind accessors of quad"
  `(with-accessors ((top-left quad-top-left)
                    (top-right quad-top-right)
                    (bottom-left quad-bottom-left)
                    (bottom-right quad-bottom-right))
       ,quad
     ,@body))

(defun split-2d-array-into-quads (array)
  (assert (= 2 (length (array-dimensions array))))
  ;; (assert (>= (array-dimension array 0) 2))
  ;; (assert (>= (array-dimension array 1) 2))
  
  (let* ((w (array-dimension array 1))
         (h (array-dimension array 0))
         (w/2 (floor (/ w 2)))
         (h/2 (floor (/ h 2)))
         )
    (make-quad :top-left (sub-2d-array array 0 0 w/2 h/2)
               :top-right (sub-2d-array array w/2 0 (- w w/2) h/2)
               :bottom-left (sub-2d-array array 0 h/2 w/2 (- h h/2))
               :bottom-right (sub-2d-array array w/2 h/2 (- w w/2) (- h h/2)))))

(defun split-2d-array-into-quads/test ()
  (declare (optimize (speed 0) (debug 3) (safety 3)))

  ;;test one
  (let ((data #2A((1  2  3  4)
                  (5  6  7  8)
                  (9  10 11 12)
                  (13 14 15 16))))
    (quad-destructure (split-2d-array-into-quads data)
      (assert (equalp top-left #2A((1 2)
                                   (5 6))))
      (assert (equalp top-right #2A((3 4)
                                    (7 8))))
      (assert (equalp bottom-left #2A((9  10)
                                      (13 14))))
      (assert (equalp bottom-right #2A((11 12)
                                       (15 16))))))
  ;;test two
  (let ((data #2A((1  2  3  4  5)
                  (6  7  8  9  10)
                  (11 12 13 14 15)
                  (16 17 18 19 20))))
    (quad-destructure (split-2d-array-into-quads data)
      (assert (equalp top-left #2A((1 2)
                                   (6 7))))
      (assert (equalp top-right #2A((3 4 5)
                                    (8 9 10))))
      (assert (equalp bottom-left #2A((11 12)
                                      (16 17))))
      (assert (equalp bottom-right #2A((13 14 15)
                                       (18 19 20))))))

  ;; test three
  (let ((data #2A((1))))
    ;;This should make sure splitting an 1x1 array doesn't error
    (assert (not (null (ignore-errors (split-2d-array-into-quads data))))))

  
  t)

(defun 2d-array-width (array)
  (assert (= 2 (length (array-dimensions array))))
  (array-dimension array 1))
(defun 2d-array-height (array)
  (assert (= 2 (length (array-dimensions array))))
  (array-dimension array 0))


(defun factor-pixels-into-list-of-colored-rectangles (pixels &optional (offset-x 0) (offset-y 0))
  (when (empty-array-p pixels)
    (return-from factor-pixels-into-list-of-colored-rectangles nil))
  (when (all-the-same-color-p pixels)
    (return-from factor-pixels-into-list-of-colored-rectangles
      (list (clgfw::make-color-rect :x offset-x :y offset-y
                                    :w (2d-array-width pixels)
                                    :h (2d-array-height pixels)
                                    :color (aref pixels 0 0)))))

  (let (result)
    (quad-destructure (split-2d-array-into-quads pixels)
      (let ((top-left-rects (factor-pixels-into-list-of-colored-rectangles
                             top-left
                             offset-x
                             offset-y))
            (top-right-rects (factor-pixels-into-list-of-colored-rectangles
                              top-right
                              (+ offset-x (2d-array-width top-left))
                              offset-y))
            (bottom-left-rects (factor-pixels-into-list-of-colored-rectangles
                                bottom-left
                                offset-x
                                (+ offset-y (2d-array-height top-left))))
            (bottom-right-rects (factor-pixels-into-list-of-colored-rectangles
                                 bottom-right
                                 (+ offset-x (2d-array-width bottom-left))
                                 (+ offset-y (2d-array-height top-right)))))

        ;; Combine neighboring rects of the same colors
        (when (and (= 1 (length top-left-rects))
                   (= 1 (length top-right-rects))
                   (= (color-rect-y (first bottom-left-rects))
                      (color-rect-y (first bottom-right-rects)))
                   (and (= (color-rect-color (first top-left-rects))
                           (color-rect-color (first top-right-rects))))
                   (and (= (color-rect-h (first top-left-rects))
                           (color-rect-h (first top-right-rects)))))
          (incf (color-rect-w (first top-left-rects)) (color-rect-w (first top-right-rects)))
          (setf top-right-rects nil))
        (when (and (= 1 (length bottom-left-rects))
                   (= 1 (length bottom-right-rects))
                   (= (color-rect-y (first bottom-left-rects))
                      (color-rect-y (first bottom-right-rects)))
                   (= (color-rect-h (first bottom-left-rects))
                      (color-rect-h (first bottom-right-rects)))
                   (= (color-rect-color (first bottom-left-rects))
                            (color-rect-color (first bottom-right-rects))))
          (incf (color-rect-w (first bottom-left-rects)) (color-rect-w (first bottom-right-rects)))
          (setf bottom-right-rects nil))

        ;; ;; FOR SOME REASON THIS PART OF THE MERGE CODE IS BROKEN SO I COMMENTED IT OUT
        ;; (when (and (= 1 (length top-left-rects))
        ;;            (= 1 (length bottom-left-rects))
        ;;            (and (= (color-rect-x (first top-left-rects))
        ;;                    (color-rect-x (first bottom-left-rects))))
        ;;            (and (= (color-rect-w (first top-left-rects))
        ;;                    (color-rect-w (first bottom-left-rects))))
        ;;            (and (= (color-rect-color (first top-left-rects))
        ;;                    (color-rect-color (first bottom-left-rects)))))
        ;;   (incf (color-rect-h (first top-left-rects)) (color-rect-h (first bottom-left-rects)))
        ;;   (setf bottom-left-rects nil))
        ;; (when (and (= 1 (length top-right-rects))
        ;;            (= 1 (length bottom-right-rects))
        ;;            (and (= (color-rect-x (first top-right-rects))
        ;;                    (color-rect-x (first bottom-right-rects))))
        ;;            (and (= (color-rect-w (first top-right-rects))
        ;;                    (color-rect-w (first bottom-right-rects))))
        ;;            (and (= (color-rect-color (first top-right-rects))
        ;;                    (color-rect-color (first bottom-right-rects)))))
        ;;   (incf (color-rect-h (first top-right-rects)) (color-rect-h (first bottom-right-rects)))
        ;;   (setf bottom-right-rects nil))

        ;; Record results
        (unless (empty-array-p top-left)
          (appendf result top-left-rects))
        (unless (empty-array-p top-right)
          (appendf result top-right-rects))
        (unless (empty-array-p bottom-left)
          (appendf result bottom-left-rects))
        (unless (empty-array-p bottom-right)
          (appendf result bottom-right-rects))
        )
      ;; (unless (empty-array-p top-left)
      ;;   (appendf result (factor-pixels-into-list-of-colored-rectangles
      ;;                    top-left
      ;;                    offset-x
      ;;                    offset-y)))
      ;; (unless (empty-array-p top-right)
      ;;   (appendf result (factor-pixels-into-list-of-colored-rectangles
      ;;                    top-right
      ;;                    (+ offset-x (2d-array-width top-left))
      ;;                    offset-y)))
      ;; (unless (empty-array-p bottom-left)
      ;;   (appendf result (factor-pixels-into-list-of-colored-rectangles
      ;;                    bottom-left
      ;;                    offset-x
      ;;                    (+ offset-y (2d-array-height top-left)))))
      ;; (unless (empty-array-p bottom-right)
      ;;   (appendf result (factor-pixels-into-list-of-colored-rectangles
      ;;                    bottom-right
      ;;                    (+ offset-x (2d-array-width bottom-left))
      ;;                    (+ offset-y (2d-array-height top-right)))))
      )

    ;; Clean up output data
    (remove-duplicates
     (remove-if (lambda (r) (or (zerop (color-rect-w r))
                                (zerop (color-rect-h r))))
                (remove-if (lambda (r) (color-invisible-p (color-rect-color r)))
                           result)))))

(defun report-compression-efficiency (pixels rects)
  (loop :for y :from 0 :below (2d-array-height pixels)
        :sum (loop :for x :from 0 :below (2d-array-width pixels)
                  :sum (if (color-invisible-p (aref pixels y x)) 0 1))
        :into total-pixels-to-draw
        :finally (format
                  t
                  ";; If pixels were rendered indiviually, it would require ~a draw calls~%"
                  total-pixels-to-draw))

  (format t ";; Drawing the compressed version only requires ~a draw calls~%" (length rects)))

(defun generate-draw-fn (ctx pixels)
  (let* ((rects (factor-pixels-into-list-of-colored-rectangles pixels))
         (draw-fn (the function (clgfw::%get-draw-rectangle-function ctx))))
    (report-compression-efficiency pixels rects)
    (lambda (ctx x y &optional tint)
      (declare (optimize (speed 3)
                         (debug 3)
                         (safety 3)))
      (let ((my-x (the fixnum (coerce x 'fixnum)))
            (my-y (the fixnum (coerce y 'fixnum))))
        (dolist (r rects)
          (funcall draw-fn ctx
                   (the fixnum (+ my-x (the fixnum (color-rect-x r))))
                   (the fixnum (+ my-y (the fixnum (color-rect-y r))))
                   (color-rect-w r)
                   (color-rect-h r)
                   (if tint
                       (color-blend (color-rect-color r) tint)
                       (color-rect-color r))))))))

(defmethod clgfw:create-sprite (ctx width height)
  (make-instance 'sprite/software-render
                 :pixels (make-array (list height width) :element-type 'color
                                     :initial-element (clgfw:make-color 0 0 0 0))))

(defmethod clgfw:draw-sprite (ctx (sprite sprite/software-render) x y &optional tint)
  (if (and (not (modifiedp sprite))
           (draw-fn sprite))
      (funcall (draw-fn sprite) ctx x y tint)
      (progn
        (format t "Regenerating sprite!~%")
        (setf (draw-fn sprite) (generate-draw-fn ctx (pixels sprite)))
        (setf (modifiedp sprite) nil)
        (draw-sprite ctx sprite x y tint))))

(defmethod get-window-width ((sprite sprite/software-render))
  (array-dimension (pixels sprite) 1))
(defmethod get-window-height ((sprite sprite/software-render))
  (array-dimension (pixels sprite) 0))


(defun %draw-rectangle/sprite (sprite x y width height color)
  (loop :for dx :from x :below (min (+ x width) (get-window-width sprite))
        :do
           (loop :for dy :from y :below (min (+ y height) (get-window-height sprite))
                 :for base-color = (aref (pixels sprite) dy dx)
                 :do (setf (aref (pixels sprite) dy dx) (color-blend base-color color)))))

(defmethod clgfw:%get-draw-rectangle-function (ctx)
  #'%draw-rectangle/sprite)

(defmethod clgfw:destroy-sprite ((sprite sprite/software-render))
  (declare (ignore sprite)))


