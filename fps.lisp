;;;; FPS.LISP
;;;; This file defines some basic functionality for calculating the current
;;;; fps of a given backend

(in-package #:clgfw)

(defconstant +ns-per-ms+ 1000000)
(defconstant +ms-per-s+ 1000)

(defun timestamp-total-ms (timestamp)
    (+
     (/ (local-time:nsec-of timestamp) +ns-per-ms+)
     (* (local-time:sec-of timestamp) +ms-per-s+)))

(defun ms-timestamp ()
  (timestamp-total-ms (local-time:now)))

(defclass fps-manager ()
  ((prior-frame :accessor prior-frame :initform nil)
   (current-frame :accessor current-frame :initform nil)
   (target-fps :initform 170 :accessor target-fps)))

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
      (return-from get-delta-time 0))
    (- (timestamp-total-ms current-frame)
       (timestamp-total-ms prior-frame))
     ;; (/ 
     ;; (-
     ;;  (local-time:nsec-of current-frame)
     ;;  (local-time:nsec-of prior-frame))
     ;; +ns-per-ms+)
    ))

(defun ms-to-fps (milliseconds)
  (if (plusp milliseconds)
      (/ 1000.0 milliseconds)
      0.0))

(defun fps-to-ms (fps)
  (/ 1000.0 fps))

(defun target-ms-per-frame (ctx)
  (fps-to-ms (target-fps ctx)))

(defmethod get-fps ((ctx fps-manager))
  (ms-to-fps (get-delta-time ctx)))

(defgeneric frame-elapsed-ms (ctx))
(defmethod frame-elapsed-ms ((ctx fps-manager))
  "Returns how many ms have passed since the frame began"
  (-
   (ms-timestamp)
   (timestamp-total-ms (current-frame ctx))))

(defun current-frame-remaining-ms-budget (ctx)
  (-
   (target-ms-per-frame ctx)
   (frame-elapsed-ms ctx)))

(defmethod end-drawing :after ((ctx fps-manager))
  "Sleep until target fps is reached"

  ;; (loop
  ;;   for remaining = (current-frame-remaining-ms-budget ctx)
  ;;   while (> remaining 1)
  ;;   do (sleep (/ (- remaining 1) 1000.0)))


  ;; (format t "current-frame-remaining-ms-budget ~a~%" (current-frame-remaining-ms-budget ctx))
  ;; (let ((budget (* 0.001 (current-frame-remaining-ms-budget ctx))))
  ;;   (when (plusp budget)
  ;;     (sleep budget)))
  ;; (format t "frame-elapsed-ms: ~a~%" (frame-elapsed-ms ctx))
  ;; (loop :while (> (current-frame-remaining-ms-budget ctx) 0)
  ;;       ;; :for i :from 0
  ;;       ;; :finally (format t "Looped ~a times~%" i)
  ;;       )
  (sleep (max 0 (/ (current-frame-remaining-ms-budget ctx) 1000)))
  )
