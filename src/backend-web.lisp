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


(defpackage #:clgfw/backend/web
  (:use #:cl #:clgfw))
(in-package #:clgfw/backend/web)

#-jscl
(error "The Web windowing backend requires JSCL")

(defparameter *key-mapping-data*
  '((key-quote           "Quote")
    (key-comma           "Comma")
    (key-minus           "Minus")
    (key-period          "Period")
    (key-slash           "Slash")

    (key-zero            "Digit0")
    (key-one             "Digit1")
    (key-two             "Digit2")
    (key-three           "Digit3")
    (key-four            "Digit4")
    (key-five            "Digit5")
    (key-six             "Digit6")
    (key-seven           "Digit7")
    (key-eight           "Digit8")
    (key-nine            "Digit9")

    (key-semicolon       "Semicolon")
    (key-equal           "Equal")

    (key-a               "KeyA")
    (key-b               "KeyB")
    (key-c               "KeyC")
    (key-d               "KeyD")
    (key-e               "KeyE")
    (key-f               "KeyF")
    (key-g               "KeyG")
    (key-h               "KeyH")
    (key-i               "KeyI")
    (key-j               "KeyJ")
    (key-k               "KeyK")
    (key-l               "KeyL")
    (key-m               "KeyM")
    (key-n               "KeyN")
    (key-o               "KeyO")
    (key-p               "KeyP")
    (key-q               "KeyQ")
    (key-r               "KeyR")
    (key-s               "KeyS")
    (key-t               "KeyT")
    (key-u               "KeyU")
    (key-v               "KeyV")
    (key-w               "KeyW")
    (key-x               "KeyX")
    (key-y               "KeyY")
    (key-z               "KeyZ")

    (key-left-bracket    "BracketLeft")
    (key-backslash       "Backslash")
    (key-right-bracket   "BracketRight")
    (key-backtick        "Backquote")

    (key-space           "Space")
    (key-escape          "Escape")
    (key-enter           "Enter")
    (key-tab             "Tab")
    (key-backspace       "Backspace")
    (key-insert          "Insert")
    (key-delete          "Delete")

    (key-right           "ArrowRight")
    (key-left            "ArrowLeft")
    (key-down            "ArrowDown")
    (key-up              "ArrowUp")
    (key-page-up         "PageUp")
    (key-page-down       "PageDown")
    (key-home            "Home")
    (key-end             "End")

    (key-caps-lock       "CapsLock")
    (key-scroll-lock     "ScrollLock")
    (key-num-lock        "NumLock")
    (key-print-screen    "PrintScreen")
    (key-pause           "Pause")

    (key-f1              "F1")
    (key-f2              "F2")
    (key-f3              "F3")
    (key-f4              "F4")
    (key-f5              "F5")
    (key-f6              "F6")
    (key-f7              "F7")
    (key-f8              "F8")
    (key-f9              "F9")
    (key-f10             "F10")
    (key-f11             "F11")
    (key-f12             "F12")

    (key-left-shift      "ShiftLeft")
    (key-right-shift     "ShiftRight")

    (key-left-control    "ControlLeft")
    (key-right-control   "ControlRight")

    (key-left-alt        "AltLeft")
    (key-right-alt       "AltRight")

    (key-left-super      "MetaLeft")
    (key-right-super     "MetaRight")

    (key-left-meta       "MetaLeft")
    (key-right-meta      "MetaRight")

    ;; KeyboardEvent.code has no distinct Hyper code.
    (key-left-hyper    nil)
    (key-right-hyper   nil)

    (key-kb-menu         "ContextMenu")

    (key-keypad-0        "Numpad0")
    (key-keypad-1        "Numpad1")
    (key-keypad-2        "Numpad2")
    (key-keypad-3        "Numpad3")
    (key-keypad-4        "Numpad4")
    (key-keypad-5        "Numpad5")
    (key-keypad-6        "Numpad6")
    (key-keypad-7        "Numpad7")
    (key-keypad-8        "Numpad8")
    (key-keypad-9        "Numpad9")
    (key-keypad-decimal  "NumpadDecimal")
    (key-keypad-divide   "NumpadDivide")
    (key-keypad-multiply "NumpadMultiply")
    (key-keypad-subtract "NumpadSubtract")
    (key-keypad-add      "NumpadAdd")
    (key-keypad-enter    "NumpadEnter")
    (key-keypad-equal    "NumpadEqual")))

(defparameter *js-key->lisp-key*
  (let ((tbl (make-hash-table :test 'equal)))
    (loop :for (lisp js) :in *key-mapping-data*
          :do (setf (gethash js tbl) lisp))
    tbl))

(defun js-key->lisp-key (jsstring)
  (gethash (jscl/ffi:clstring jsstring) *js-key->lisp-key*))


(defclass backend/web ()
  ((canvas-node :reader canvas-node)
   (canvas-ctx :reader canvas-ctx)
   (quit-requested-p
    :reader quit-requested-p :initform nil)
   (text-height :reader text-height :initform 10)
   (callback-handler :reader callback-handler)))

(register-backend 'backend/web +priority-native+)

;;TODO support this
;; (defgeneric callback-on-window-resize (handler width height))

(defmethod %backend-window-create ((ctx backend/web) width height
                                   title callback-handler-instance)
  (setf (slot-value ctx 'callback-handler)
        callback-handler-instance)
  
  (let* ((canvas-node (#j:document:createElement #j"canvas")))

    (#j:document:body:append canvas-node)
    (setf (jscl/ffi:oget canvas-node "width") width)
    (setf (jscl/ffi:oget canvas-node "height") height)

    ;; Add canvas border
    (setf (jscl/ffi:oget canvas-node "style")  #j"border: 2px solid darkgray;")
    
    (setf (slot-value ctx 'canvas-ctx)
          ((jscl/ffi:oget canvas-node "getContext")
           (jscl/ffi:jsstring "2d")))
    (setf (slot-value ctx 'canvas-node) canvas-node)
    
    (setf #j:document:title (jscl/ffi:jsstring title)))

  ;; register event handlers
  (flet ((on-mouse-move (e)
           (%callback-on-mouse-move callback-handler-instance
                                    (jscl/ffi:oget e "clientX")
                                    (jscl/ffi:oget e "clientY")))
         (on-mouse-down (e)
           (%callback-on-mouse-down callback-handler-instance
                                    (let ((btn (jscl/ffi:oget e "button")))
                                      (cond ((= btn 0) :left)
                                            ((= btn 1) :middle)
                                            ((= btn 2) :right)))))
         (on-mouse-up (e)
           (%callback-on-mouse-up callback-handler-instance
                                  (let ((btn (jscl/ffi:oget e "button")))
                                    (cond ((= btn 0) :left)
                                          ((= btn 1) :middle)
                                          ((= btn 2) :right)))))
         (on-key-down (e)
           (%callback-on-key-down callback-handler-instance
                                  (js-key->lisp-key (jscl/ffi:oget e "code"))))
         (on-key-up (e)
           (%callback-on-key-up callback-handler-instance
                                (js-key->lisp-key (jscl/ffi:oget e "code")))))

    (#j:document:addEventListener #j"mousemove" #'on-mouse-move)
    (#j:document:addEventListener #j"mousedown" #'on-mouse-down)
    (#j:document:addEventListener #j"mouseup" #'on-mouse-up)
    (#j:document:addEventListener #j"keydown" #'on-key-down)
    (#j:document:addEventListener #j"keyup" #'on-key-up))

  ;; Notify of canvas size
  (%callback-on-window-resize callback-handler-instance width height)

  ctx)

;; (defmethod backend-close-window ((ctx backend/web))
;;   ((jscl/ffi:oget (slot-value ctx 'canvas-node) "remove")))

;; (defmethod backend-begin-drawing ((ctx backend/web))
;;   (error "Calling begin-drawing directly is not supported on the
;;            web backend. Use with-drawing instead."))

;; (defmethod backend-end-drawing ((ctx backend/web))
;;   (error "Calling end-drawing directly is not supported on the
;;            web backend. Use with-drawing instead."))

(defun color->jsstring (color)
  (jscl/ffi:jsstring (format nil "rgba(~a, ~a, ~a, ~a)" 
                             (color-r color)
                             (color-g color)
                             (color-b color)
                             (color-a color))))

(defmethod %backend-draw-rectangle ((ctx backend/web) x y w h color
                                    &key angle (origin-x 0) (origin-y 0) target)
  (let ((context-2d (if target
                        ((jscl/ffi:oget target "getContext") #j"2d")
                        (slot-value ctx 'canvas-ctx))))

    ;; begin path 
    (setf (jscl/ffi:oget context-2d "fillStyle")
          (color->jsstring color))
    ((jscl/ffi:oget context-2d "beginPath"))
    
    ;; translate canvas and rotate when necessary, then draw rect
    (cond ((and angle (not (= angle 0)))
           ((jscl/ffi:oget context-2d "save"))
           ((jscl/ffi:oget context-2d "translate")
            (+ x origin-x)
            (+ y origin-y))
           ((jscl/ffi:oget context-2d "rotate")
            (* angle #.(/ pi 180)))
           ((jscl/ffi:oget context-2d "fillRect") 0 0 w h)
           ((jscl/ffi:oget context-2d "restore")))

          ;;else
          (t ((jscl/ffi:oget context-2d "fillRect")
              (+ x origin-x)
              (+ y origin-y) w h)))))

(defmethod %backend-set-preferred-text-height ((ctx backend/web) text-height)
  (setf (slot-value ctx 'text-height) text-height))

(defmethod %backend-get-text-height ((ctx backend/web))
  (slot-value ctx 'text-height))

(defmethod %backend-request-quit ((ctx backend/web))
  (setf (slot-value ctx 'quit-requested-p) t))

(defmethod %backend-measure-text-width ((ctx backend/web) text)
  (jscl/ffi:oget ((jscl/ffi:oget (canvas-ctx ctx) "measureText") text)
                 "width"))

(defmethod %backend-draw-text  ((ctx backend/web) text x y color
                                &key (angle 0) (origin-x 0) (origin-y 0) target)
  (let ((context-2d (if target
                        ((jscl/ffi:oget target "getContext") #j"2d")
                        (slot-value ctx 'canvas-ctx))))

    ;; begin path 
    (setf (jscl/ffi:oget context-2d "fillStyle")
          (color->jsstring color))

    (setf (jscl/ffi:oget (slot-value ctx 'canvas-ctx) "font")
          (jscl/ffi:jsstring (format nil "~apx sans-serif"
                                     (slot-value ctx 'text-height))))
    ((jscl/ffi:oget context-2d "beginPath"))
    
    ;; translate canvas and rotate when necessary, then draw rect
    (cond ((and angle (not (= angle 0)))
           ((jscl/ffi:oget context-2d "save"))
           ((jscl/ffi:oget context-2d "translate")
            (+ x origin-x)
            (+ y origin-y (slot-value ctx 'text-height)))
           ((jscl/ffi:oget context-2d "rotate")
            (* angle #.(/ pi 180)))
           ((jscl/ffi:oget (canvas-ctx ctx) "fillText") (jscl/ffi:jsstring text)
            0 0)
           ((jscl/ffi:oget context-2d "restore")))

          ;;else
          (t ((jscl/ffi:oget (canvas-ctx ctx) "fillText") (jscl/ffi:jsstring text)
              (+ x origin-x)
              (+ y origin-y (slot-value ctx 'text-height)))))))

(defmethod %backend-draw-canvas ((ctx backend/web) canvas dst-x dst-y
                                 &key angle origin-x origin-y tint target
                                   dst-w dst-h
                                   src-x src-y src-w src-h)

  (let ((target-canvas (or target (slot-value ctx 'canvas-ctx))))
    ;; TODO handle tint
    ;; TODO handle w and h
    ;; TODO handle angle and origin-x/y
    ((jscl/ffi:oget target-canvas "drawImage") canvas dst-x dst-y)))

(defmethod %backend-create-canvas ((ctx backend/web) w h)
  (let* ((new-canvas-node (#j:document:createElement #j"canvas")))

    ;; todo store a reference to this canvas in our backend somewhere so we
    ;; can delete it later if we need
    (setf (jscl/ffi:oget new-canvas-node "width") w)
    (setf (jscl/ffi:oget new-canvas-node "height") h)
    (setf (jscl/ffi:oget new-canvas-node "style" "display") #j"none")
    (#J:document:body:append new-canvas-node)
    new-canvas-node))

(defmethod %backend-destroy-canvas ((ctx backend/web) canvas)
  ((jscl/ffi:oget canvas "remove")))

(defmethod %backend-check-for-input ((ctx backend/web))
  (slot-value ctx 'input-happened-p))

(defmethod %backend-window-run ((ctx backend/web) draw-function-callback)
  (labels ((raw-callback (timestamp)
             (%callback-on-frame-begin
              (slot-value ctx 'callback-handler))
             (funcall draw-function-callback)
             (%callback-on-frame-end
              (slot-value ctx 'callback-handler))
             (if (not (quit-requested-p ctx))
                 (#j:window:requestAnimationFrame #'raw-callback)
                 ;; TODO: CLOSE WINDOW HEREN
                 )))
     (#j:window:requestAnimationFrame #'raw-callback)))


;; TODO
(defmethod %backend-clipboard-get ((ctx backend/web)))
(defmethod %backend-clipboard-set ((ctx backend/web) string))

(defmethod %backend-scissor-begin ((ctx backend/web) x y w h))
(defmethod %backend-scissor-end ((ctx backend/web)))
