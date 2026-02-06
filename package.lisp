(uiop:define-package #:clgfw
  (:use #:cl #:alexandria)
  (:export #:init-window
           #:close-window
           #:window-should-keeping-running-p
           #:window-should-close-p
           #:begin-drawing
           #:end-drawing
           #:draw-rectangle
           #:set-preferred-text-height
           #:draw-text
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
           #:make-color
           #:color
           #:color-r
           #:color-g
           #:color-b
           #:color-a
           #:color-invisible-p
           #:color-opaque-p
           #:color-blend
           #:char->key
           #:key->char
           #:button
           #:key

           ;; These are just wrappers around init-window/close-window, etc...
           #:with-window
           #:with-drawing
           #:while-running

           ;; For Writing New Backends
           #:*backends*
           #:backend-init-window              
           #:backend-close-window             
           #:backend-window-should-close-p    
           #:backend-begin-drawing            
           #:backend-end-drawing              
           #:backend-draw-rectangle           
           #:backend-set-preferred-text-height
           #:backend-get-text-height          
           #:backend-measure-text-width       
           #:backend-draw-text                
           #:backend-draw-canvas              
           #:backend-create-canvas            
           #:backend-destroy-canvas           
           #:backend-canvas-draw-rectangle    
           #:backend-canvas-draw-text         
           #:backend-canvas-draw-canvas

           ;; Callbacks a backend should call to update clgfw about user input
           #:callback-on-mouse-move   
           #:callback-on-mouse-down   
           #:callback-on-mouse-up     
           #:callback-on-key-down     
           #:callback-on-key-up       
           #:callback-on-window-resize))
