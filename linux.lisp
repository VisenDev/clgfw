;;;; The primary purpose of this file is to toggle between wayland or x11 backends

(in-package #:clgfw)

(defparameter *linux-init-window-functions* '(init-window/wayland init-window/x11))

(defun init-window/linux (width height title)
  "Attempts to initialize a window on linux using both wayland and x11"
  (handler-case (funcall (first *linux-init-window-functions*) width height title)
    (error (e)
      (format t "~a~%" e)
      (format t "Failed to launch window using ~a, falling back to ~a~%"
              (first *linux-init-window-functions*)
              (second *linux-init-window-functions*))
      (funcall (second *linux-init-window-functions*) width height title))))
