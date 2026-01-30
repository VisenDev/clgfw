;;;; PIXEL-BUFFER.LISP
;;;; This file implements a basic pixel buffer to
;;;; be used as a backend for image rendering
;;;;
;;;; By Robert Burnett
;;;; Copyright 2026
;;;;
;;;; Licensed under Apache-2.0

(defpackage #:clgfw/pixel-buffer
  (:use #:cl #:clgfw))
(in-package #:clgfw/pixel-buffer)

(defclass pixel-buffer ()
  ((pixels :accessor pixels :type (simple-array color))
   (width :accessor width)
   (height :accessor height)
   (draw-fn :accessor draw-fn :initform nil)))

(defun generate-draw-fn ()
  
  )

(defun render-pixel-buffer

  )

