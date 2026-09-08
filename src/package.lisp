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

(defpackage #:clgfw
  (:use #:cl)
  (:export #:window-create
           #:window-run
           #:request-quit
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

           ;; Color Related Datatypes
           #:u8
           #:normalized-float

           ;; RGBA
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
           #:color-tint
           #:color->normalized-color
           #:color->luminance
           #:color->xrgb

           ;; XRGB
           #:xrgb
           #:xrgb->color

           ;; NORMALIZED-COLORS
           #:normalized-color
           #:normalized-color->color
           #:normalized-color->luminance
           #:norm-color-r
           #:norm-color-g
           #:norm-color-b
           #:norm-color-a

           ;; LUMINANCE
           #:luminance
           #:luminance->normalized-color
           #:luminance->color
           #:+luminance-red+
           #:+luminance-green+
           #:+luminance-blue+
           

           ;; FPS and redraw related symbols
           #:get-fps
           #:get-fps-string
           #:get-delta-time
           #:set-redraw-frequency
           #:draw-fps

           ;; IO Related symbols
           #:char->key
           #:key->char
           #:button
           #:key

           ;; For Writing New Backends
           #:register-backend
           #:unregister-all-backends
           #:+priority-native+
           #:+priority-primary+
           #:+priority-secondary+
           #:+priority-last+
           #:%backend-window-run
           #:%backend-window-create
           #:%backend-draw-rectangle
           #:%backend-set-preferred-text-height
           #:%backend-get-text-height
           #:%backend-measure-text-width
           #:%backend-draw-text
           #:%backend-draw-canvas
           #:%backend-create-canvas
           #:%backend-destroy-canvas
           #:%backend-check-for-input
           #:%backend-clipboard-get
           #:%backend-clipboard-set
           #:%backend-scissor-begin
           #:%backend-scissor-end
           #:%backend-request-quit
           #:%backend-blit                 
           #:%backend-read-pixels          
           #:%backend-gamepads-list        
           #:%backend-gamepad-name         
           #:%backend-gamepad-button-down-p
           #:%backend-gamepad-axis-read    

           ;; Callbacks a backend should call to update clgfw about user input
           #:%callback-on-mouse-move
           #:%callback-on-mouse-down
           #:%callback-on-mouse-up
           #:%callback-on-key-down
           #:%callback-on-key-up
           #:%callback-on-window-resize
           #:%callback-on-frame-begin
           #:%callback-on-frame-end


           ;; Key and mouse codes
           #:mouse-button-left
           #:mouse-button-right
           #:mouse-button-middle
           #:key-quote          
           #:key-comma                     
           #:key-minus          
           #:key-period         
           #:key-slash          
           #:key-zero           
           #:key-one            
           #:key-two            
           #:key-three          
           #:key-four           
           #:key-five           
           #:key-six            
           #:key-seven          
           #:key-eight          
           #:key-nine           
           #:key-semicolon      
           #:key-equal          
           #:key-a              
           #:key-b              
           #:key-c              
           #:key-d              
           #:key-e              
           #:key-f              
           #:key-g              
           #:key-h              
           #:key-i              
           #:key-j              
           #:key-k              
           #:key-l              
           #:key-m              
           #:key-n              
           #:key-o              
           #:key-p              
           #:key-q              
           #:key-r              
           #:key-s              
           #:key-t              
           #:key-u              
           #:key-v              
           #:key-w              
           #:key-x              
           #:key-y              
           #:key-z              
           #:key-left-bracket   
           #:key-backslash      
           #:key-right-bracket  
           #:key-backtick       
           #:key-space          
           #:key-escape         
           #:key-enter          
           #:key-tab            
           #:key-backspace      
           #:key-insert         
           #:key-delete         
           #:key-right          
           #:key-left           
           #:key-down           
           #:key-up             
           #:key-page-up        
           #:key-page-down      
           #:key-home           
           #:key-end            
           #:key-caps-lock      
           #:key-scroll-lock    
           #:key-num-lock       
           #:key-print-screen   
           #:key-pause          
           #:key-f1             
           #:key-f2             
           #:key-f3             
           #:key-f4             
           #:key-f5             
           #:key-f6             
           #:key-f7             
           #:key-f8             
           #:key-f9             
           #:key-f10            
           #:key-f11            
           #:key-f12            
           #:key-left-shift     
           #:key-right-shift    
           #:key-left-control   
           #:key-right-control  
           #:key-left-alt       
           #:key-right-alt      
           #:key-left-super     
           #:key-right-super    
           #:key-left-meta      
           #:key-right-meta     
           #:key-left-hyper     
           #:key-right-hyper    
           #:key-kb-menu        
           #:key-keypad-0       
           #:key-keypad-1       
           #:key-keypad-2       
           #:key-keypad-3       
           #:key-keypad-4       
           #:key-keypad-5       
           #:key-keypad-6       
           #:key-keypad-7       
           #:key-keypad-8       
           #:key-keypad-9       
           #:key-keypad-decimal 
           #:key-keypad-divide  
           #:key-keypad-multiply
           #:key-keypad-subtract
           #:key-keypad-add     
           #:key-keypad-enter   
           #:key-keypad-equal
           
           #:gamepad-button-south
           #:gamepad-button-east
           #:gamepad-button-west
           #:gamepad-button-north
           #:gamepad-button-left-bumper
           #:gamepad-button-right-bumper
           #:gamepad-button-left-trigger
           #:gamepad-button-right-trigger
           #:gamepad-button-select
           #:gamepad-button-start
           #:gamepad-button-guide
           #:gamepad-button-left-stick
           #:gamepad-button-right-stick
           #:gamepad-button-dpad-up
           #:gamepad-button-dpad-right
           #:gamepad-button-dpad-down
           #:gamepad-button-dpad-left
           #:gamepad-axis-left-x
           #:gamepad-axis-left-y
           #:gamepad-axis-right-x
           #:gamepad-axis-right-y
           #:gamepad-axis-left-trigger
           #:gamepad-axis-right-trigger
           #:gamepad-button
           #:gamepad-axis))
