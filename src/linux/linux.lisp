;;;; The primary purpose of this file is to toggle between wayland or x11 backends

(defpackage #:clgfw/linux
  (:use #:cl #:clgfw/common)
  )
(in-package #:clgfw/linux)

(defun init-window (width height title)
  (handler-case (clgfw/x11:init-window width height title)
    (error ()
      (clgfw/wayland:init-window width height title))))

