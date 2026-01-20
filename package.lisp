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
           #:is-key-pressed
           #:get-window-width
           #:get-window-height

           ;; Common functions between implementations
           #:make-color
           #:color-r
           #:color-g
           #:color-b

           ;; Utility wrappers
           #:with-window
           #:with-drawing
           #:while-running
           #:while-running/with-drawing))
(in-package #:clgfw)

