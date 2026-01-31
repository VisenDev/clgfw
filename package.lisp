(defpackage #:clgfw
  (:use #:cl
        #+(and linux (not abcl)) #:wayflan
        #+(and linux (not abcl)) #:wayflan-client
        #+(and linux (not abcl)) #:wayflan-client.xdg-shell)
  (:export #:init-window
           #:close-window
           #:window-should-keeping-running
           #:begin-drawing
           #:end-drawing
           #:draw-rectangle
           #:%get-draw-rectangle-function
           #:draw-text
           #:draw-image
           #:create-image
           #:destroy-image
           #:get-mouse-x
           #:get-mouse-y
           #:is-mouse-button-down
           #:is-key-down
           #:is-key-up
           #:is-key-pressed
           #:is-key-released
           #:get-window-width
           #:get-window-height
           #:get-fps
           #:get-delta-time
           #:set-target-fps

           ;; Common functions between implementations
           #:make-color
           #:color
           #:color-r
           #:color-g
           #:color-b
           #:color-a
           #:color-invisible-p
           
           #:color-rect-x
           #:color-rect-y
           #:color-rect-w
           #:color-rect-h
           #:color-rect-color
           #:make-color-rect
           #:color-rect

           ;; Utility wrappers
           #:with-window
           #:with-drawing
           #:while-running
           #:while-running/with-drawing

           ;; Anaphoric Macros
           #:when-it
           #:unless-it
           #:if-it

           ;; Useful macros
           #:appendf
           ))
(in-package #:clgfw)

