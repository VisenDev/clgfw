(defpackage #:clgfw
  (:use #:cl #:clgfw/common)
  (:export #:init-window
           #:close-window
           #:window-should-keeping-running
           #:begin-drawing
           #:end-drawing
           #:draw-rectangle
           #:get-mouse-x
           #:get-mouse-y
           #:is-mouse-button-down
           #:get-window-width
           #:get-window-height))
(in-package #:clgfw)
