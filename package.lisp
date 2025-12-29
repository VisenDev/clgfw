(defpackage #:clgfw
  (:use #:cl)
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
           #:get-window-height

           ;; Utility wrappers
           #:with-window
           #:with-drawing
           #:while-running
           #:while-running/with-drawing))
(in-package #:clgfw)
