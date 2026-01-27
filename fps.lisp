;;;; FPS.LISP
;;;; This file defines some basic functionality for calculating the current
;;;; fps of a given backend

(in-package #:clgfw)

(declaim (ftype (function () number) ms-timestamp))
(defun ms-timestamp ()
  (local-time:timestamp-millisecond (local-time:now)))

(defclass fps-manager ()
  ((prior-frame :accessor current-frame :initform nil)
   (current-frame :accessor current-frame :initform nil)
   (target-fps :initform 60 :accessor target-fps))
  )

(defun target-ms-per-frame (ctx)
  (fps-to-ms (target-fps ctx)))

(defmethod begin-drawing :after ((ctx fps-manager))
  (with-slots (prior-frame current-frame) ctx
    (when current-frame
      (setf prior-frame current-frame))
    (setf current-frame (local-time:now))))

(defmethod get-delta-time ((ctx fps-manager))
  "Returns ms since last frame in milliseconds"
  (with-slots (prior-frame current-frame) ctx
    (unless (and prior-frame current-frame)
      (format t "missing prior frame~%")
      (return-from get-delta-time 1))
    (-
     (local-time:timestamp-millisecond current-frame)
     (local-time:timestamp-millisecond prior-frame))))

(defun ms-to-fps (milliseconds)
  (/ 1000f0 (max milliseconds 1)))

(defun fps-to-ms (fps)
  (/ 1000f0 fps))

(defmethod get-fps ((ctx fps-manager))
  (ms-to-fps (get-delta-time ctx)))

(defgeneric frame-elapsed-ms (ctx))
(defmethod frame-elapsed-ms ((ctx fps-manager))
  "Returns how many ms have passed since the frame began"
  (-
   (ms-timestamp)
   (local-time:timestamp-millisecond (current-frame ctx))))

(defun current-frame-remaining-ms-budget (ctx)
  (-
   (target-ms-per-frame ctx)
   (frame-elapsed-ms ctx)))

(defmethod end-drawing :after ((ctx fps-manager))
  "Sleep until target fps is reached"
  (loop :while (> (current-frame-remaining-ms-budget ctx) 0)
        :for i :from 0
        :finally (format t "Looped ~a times~%" i))
  ;; (sleep (max 0 (/ (current-frame-remaining-ms-budget ctx) 1000)))
  )
