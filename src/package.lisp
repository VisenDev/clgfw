;;;; Copyright 2026 Robert Wess Burnett
;;;;
;;;; Licensed under the Apache License, Version 2.0 (the "License");
;;;; you may not use this file except in compliance with the License.
;;;; You may obtain a copy of the License at
;;;;
;;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;; Unless required by applicable law or agreed to in writing, software
;;;; distributed under the License is distributed on an "AS IS" BASIS,
;;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;;; See the License for the specific language governing permissions and
;;;; limitations under the License.

#+sbcl (setq sb-ext:*block-compile-default* t)

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
           #:draw-canvas
           #:create-canvas
           #:destroy-canvas
           #:get-mouse-x
           #:get-mouse-y
           #:is-mouse-button-down
           #:is-key-down
           #:is-key-up
           #:is-key-pressed
           #:is-key-released
           #:get-window-width
           #:get-window-height

           ;; FPS and redraw related symbols
           #:get-fps
           #:get-fps-string
           #:get-delta-time
           #:set-redraw-frequency
           #:draw-fps

           ;; Color Related Symbols
           #:make-color
           #:color
           #:color-r
           #:color-g
           #:color-b
           #:color-a
           #:color-invisible-p
           #:color-opaque-p
           #:color-premultiply-alpha
           #:color-blend
           #:xrgb
           #:color->xrgb
           #:xrgb->color

           ;; IO Related symbols
           #:char->key
           #:key->char
           #:button
           #:key

           ;; These are just wrappers around init-window/close-window, etc...
           #:with-window
           #:with-drawing
           #:while-running
           #:with-canvas
           #:with-canvases
           #:with-drawing-on-canvas

           ;; For Writing New Backends
           #:register-backend
           #:unregister-all-backends
           #:+priority-native+
           #:+priority-primary+
           #:+priority-secondary+
           #:+priority-last+
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
           #:backend-check-for-input
           #:backend-draw-rectangle-on-canvas
           #:backend-draw-text-on-canvas
           #:backend-draw-canvas-on-canvas

           ;; Callbacks a backend should call to update clgfw about user input
           #:callback-on-mouse-move
           #:callback-on-mouse-down
           #:callback-on-mouse-up
           #:callback-on-key-down
           #:callback-on-key-up
           #:callback-on-window-resize
           #:callback-all-keys-up

           ;; Html Color Constants
           #:+IndianRed+ #:+LightCoral+ #:+Salmon+ #:+DarkSalmon+          
           #:+LightSalmon+ #:+Crimson+ #:+Red+ #:+FireBrick+           
           #:+DarkRed+ #:+Pink+ #:+LightPink+ #:+HotPink+             
           #:+DeepPink+ #:+MediumVioletRed+ #:+PaleVioletRed+ #:+LightSalmon+         
           #:+Coral+ #:+Tomato+ #:+OrangeRed+ #:+DarkOrange+          
           #:+Orange+ #:+Gold+ #:+Yellow+ #:+LightYellow+         
           #:+LemonChiffon+ #:+LightGoldenrodYellow+ #:+PapayaWhip+ #:+Moccasin+            
           #:+PeachPuff+ #:+PaleGoldenrod+ #:+Khaki+ #:+DarkKhaki+           
           #:+Lavender+ #:+Thistle+ #:+Plum+ #:+Violet+              
           #:+Orchid+ #:+Fuchsia+ #:+Magenta+ #:+MediumOrchid+        
           #:+MediumPurple+ #:+RebeccaPurple+ #:+BlueViolet+ #:+DarkViolet+          
           #:+DarkOrchid+ #:+DarkMagenta+ #:+Purple+ #:+Indigo+              
           #:+SlateBlue+ #:+DarkSlateBlue+ #:+MediumSlateBlue+ #:+GreenYellow+         
           #:+Chartreuse+ #:+LawnGreen+ #:+Lime+ #:+LimeGreen+           
           #:+PaleGreen+ #:+LightGreen+ #:+MediumSpringGreen+ #:+SpringGreen+         
           #:+MediumSeaGreen+ #:+SeaGreen+ #:+ForestGreen+ #:+Green+               
           #:+DarkGreen+ #:+YellowGreen+ #:+OliveDrab+ #:+Olive+               
           #:+DarkOliveGreen+ #:+MediumAquamarine+ #:+DarkSeaGreen+ #:+LightSeaGreen+       
           #:+DarkCyan+ #:+Teal+ #:+Aqua+ #:+Cyan+                
           #:+LightCyan+ #:+PaleTurquoise+ #:+Aquamarine+ #:+Turquoise+           
           #:+MediumTurquoise+ #:+DarkTurquoise+ #:+CadetBlue+ #:+SteelBlue+           
           #:+LightSteelBlue+ #:+PowderBlue+ #:+LightBlue+ #:+SkyBlue+             
           #:+LightSkyBlue+ #:+DeepSkyBlue+ #:+DodgerBlue+ #:+CornflowerBlue+      
           #:+MediumSlateBlue+ #:+RoyalBlue+ #:+Blue+ #:+MediumBlue+          
           #:+DarkBlue+ #:+Navy+ #:+MidnightBlue+ #:+Cornsilk+            
           #:+BlanchedAlmond+ #:+Bisque+ #:+NavajoWhite+ #:+Wheat+               
           #:+BurlyWood+ #:+Tan+ #:+RosyBrown+ #:+SandyBrown+          
           #:+Goldenrod+ #:+DarkGoldenrod+ #:+Peru+ #:+Chocolate+           
           #:+SaddleBrown+ #:+Sienna+ #:+Brown+ #:+Maroon+              
           #:+White+ #:+Snow+ #:+HoneyDew+ #:+MintCream+           
           #:+Azure+ #:+AliceBlue+ #:+GhostWhite+ #:+WhiteSmoke+          
           #:+SeaShell+ #:+Beige+ #:+OldLace+ #:+FloralWhite+         
           #:+Ivory+ #:+AntiqueWhite+ #:+Linen+ #:+LavenderBlush+       
           #:+MistyRose+ #:+Gainsboro+ #:+LightGray+ #:+Silver+              
           #:+DarkGray+ #:+Gray+ #:+DimGray+ #:+LightSlateGray+      
           #:+SlateGray+ #:+DarkSlateGray+ #:+Black+
           
           ;; My custom colors
           #:+Moon+ #:+Space+
           ))
