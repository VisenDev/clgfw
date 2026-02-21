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
           #:callback-all-keys-up))


