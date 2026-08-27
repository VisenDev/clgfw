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


(in-package #:clgfw)

#-jscl
(error "The Web windowing backend requires JSCL")

(defclass backend/web ()
  ((canvas-node :accessor canvas-node)))
(register-backend 'backend/web +priority-native+)

;; (defgeneric callback-on-mouse-move    (handler x y))
;; (defgeneric callback-on-mouse-down    (handler mouse-button))
;; (defgeneric callback-on-mouse-up      (handler mouse-button))
;; (defgeneric callback-on-key-down      (handler key))
;; (defgeneric callback-on-key-up        (handler key))
;; (defgeneric callback-on-window-resize (handler width height))
;; (defgeneric callback-all-keys-up      (handler))

(defmethod backend-init-window ((ctx backend/web) width height
                                title callback-handler-instance)
  (let* ((canvas-node (#j:document:createElement #j"canvas")))
    (setf (jscl/ffi:oget canvas-node "width") width)
    (setf (jscl/ffi:oget canvas-node "height") height)
    (setf #j:document:title () (jscl/ffi:jsstring title))
    (#j:document:body:append canvas-node)
    (setf (canvas-node ctx) canvas-node)))
;; (defgeneric backend-close-window              (ctx))
;; (defgeneric backend-window-should-close-p     (ctx))
;; (defgeneric backend-begin-drawing             (ctx))
;; (defgeneric backend-end-drawing               (ctx))
;; (defgeneric backend-draw-rectangle            (ctx x y w h color))
;; (defgeneric backend-set-preferred-text-height (ctx text-height))
;; (defgeneric backend-get-text-height           (ctx))
;; (defgeneric backend-measure-text-width        (ctx text))
;; (defgeneric backend-draw-text                 (ctx x y color text))
;; (defgeneric backend-draw-canvas               (ctx x y canvas &optional tint))
;; (defgeneric backend-create-canvas             (ctx w h))
;; (defgeneric backend-destroy-canvas            (ctx canvas))
;; (defgeneric backend-check-for-input           (ctx))
;; (defgeneric backend-draw-rectangle-on-canvas  (ctx canvas x y w h color))
;; (defgeneric backend-draw-text-on-canvas       (ctx canvas x y color text))
;; (defgeneric backend-draw-canvas-on-canvas     (ctx canvas x y w h &optional tint))

