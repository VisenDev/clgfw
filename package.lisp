(uiop:define-package #:clgfw
  (:use #:cl
        #+(and linux (not abcl)) #:wayflan
        #+(and linux (not abcl)) #:wayflan-client
        #+(and linux (not abcl)) #:wayflan-client.xdg-shell)
  (:export #:init-window
           #:close-window
           #:window-should-keeping-running-p
           #:window-should-close-p
           #:begin-drawing
           #:end-drawing
           #:draw-rectangle
           #:set-preferred-text-height
           #:draw-text
           #:draw-sprite
           #:create-sprite
           #:destroy-sprite
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
           #:color-opaque-p
           #:color-blend

           ;; IO
           #:char->key
           #:key->char
           #:button
           #:key

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

           ;; Utility wrappers
           #:with-window
           #:with-drawing
           #:while-running

           ;; Anaphoric Macros
           #:when-it
           #:unless-it
           #:if-it

           ;; Useful macros
           #:appendf
           ))
(in-package #:clgfw)

