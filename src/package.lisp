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

           ;; Callbacks a backend should call to update clgfw about user input
           #:%callback-on-mouse-move
           #:%callback-on-mouse-down
           #:%callback-on-mouse-up
           #:%callback-on-key-down
           #:%callback-on-key-up
           #:%callback-on-window-resize
           #:%callback-on-frame-begin
           #:%callback-on-frame-end))


