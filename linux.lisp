;;;; The primary purpose of this file is to toggle between wayland or x11 backends

(in-package #:clgfw)

(defparameter *linux-backend* :x11)

(defun init-window (width height title)
  (ecase *linux-backend*
    (:x11 (init-window/x11 width height title))
    (:wayland (init-window/wayland width height title)))
  )
