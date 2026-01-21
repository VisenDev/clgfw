;;;; The primary purpose of this file is to toggle between wayland or x11 backends

(in-package #:clgfw)

;; (defparameter *linux-backend* :wayland)

(defun init-window (width height title)
  "Attempts to initialize a wayland window, then tries x11 if that doesn't work"
  (handler-case (init-window/wayland width height title)
    (error ()
      (init-window/x11 width height title)

      ))
  ;; (ecase *linux-backend*
  ;;   (:x11 (init-window/x11 width height title))
  ;;   (:wayland (init-window/wayland width height title)))
  )
