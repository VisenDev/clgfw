;;;; KITTY-GRAPHICS.LISP
;;;; An Experimental backend for clgfw
;;;; using the kitty graphics protocol
;;;;
;;;; Copyright 2026, Robert Wess Burnett
;;;; Licensed under Apache-2.0


(defpackage #:clgfw/kitty-graphics
  (:use #:cl)
  (:export #:backend/kitty-graphics))
(in-package #:clgfw/kitty-graphics)

;;; References
;;; https://en.wikipedia.org/wiki/ANSI_escape_code
;;; https://sw.kovidgoyal.net/kitty/graphics-protocol/

;;; ANSI
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun esc (string &rest args)
    (format nil (format nil "~a[~a" #\Esc string) args)))

(defun cursor-up ()
  (print #.(esc "1A")))


;;; KITTY
(defclass backend/kitty-graphics ()
  ((handler :accessor handler)))

(defmethod clgfw:backend-init-window
    ((ctx backend/kitty-graphics) width height title callback-handler-instance)
  (setf (handler ctx) callback-handler-instance)
  ctx)

(defmethod clgfw:backend-draw-text ((ctx backend/kitty-graphics) x y color text)
  ()
  )
