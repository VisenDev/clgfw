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
           #:with-window
           #:with-drawing
           #:while-running
           #:while-running/with-drawing))
(in-package #:clgfw)
