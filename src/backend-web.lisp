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

(defparameter *key-mapping-data*
  '((:quote           "Quote")
    (:comma           "Comma")
    (:minus           "Minus")
    (:period          "Period")
    (:slash           "Slash")

    (:zero            "Digit0")
    (:one             "Digit1")
    (:two             "Digit2")
    (:three           "Digit3")
    (:four            "Digit4")
    (:five            "Digit5")
    (:six             "Digit6")
    (:seven           "Digit7")
    (:eight           "Digit8")
    (:nine            "Digit9")

    (:semicolon       "Semicolon")
    (:equal           "Equal")

    (:a               "KeyA")
    (:b               "KeyB")
    (:c               "KeyC")
    (:d               "KeyD")
    (:e               "KeyE")
    (:f               "KeyF")
    (:g               "KeyG")
    (:h               "KeyH")
    (:i               "KeyI")
    (:j               "KeyJ")
    (:k               "KeyK")
    (:l               "KeyL")
    (:m               "KeyM")
    (:n               "KeyN")
    (:o               "KeyO")
    (:p               "KeyP")
    (:q               "KeyQ")
    (:r               "KeyR")
    (:s               "KeyS")
    (:t               "KeyT")
    (:u               "KeyU")
    (:v               "KeyV")
    (:w               "KeyW")
    (:x               "KeyX")
    (:y               "KeyY")
    (:z               "KeyZ")

    (:left-bracket    "BracketLeft")
    (:backslash       "Backslash")
    (:right-bracket   "BracketRight")
    (:backtick        "Backquote")

    (:space           "Space")
    (:escape          "Escape")
    (:enter           "Enter")
    (:tab             "Tab")
    (:backspace       "Backspace")
    (:insert          "Insert")
    (:delete          "Delete")

    (:right           "ArrowRight")
    (:left            "ArrowLeft")
    (:down            "ArrowDown")
    (:up              "ArrowUp")
    (:page-up         "PageUp")
    (:page-down       "PageDown")
    (:home            "Home")
    (:end             "End")

    (:caps-lock       "CapsLock")
    (:scroll-lock     "ScrollLock")
    (:num-lock        "NumLock")
    (:print-screen    "PrintScreen")
    (:pause           "Pause")

    (:f1              "F1")
    (:f2              "F2")
    (:f3              "F3")
    (:f4              "F4")
    (:f5              "F5")
    (:f6              "F6")
    (:f7              "F7")
    (:f8              "F8")
    (:f9              "F9")
    (:f10             "F10")
    (:f11             "F11")
    (:f12             "F12")

    (:left-shift      "ShiftLeft")
    (:right-shift     "ShiftRight")

    (:left-control    "ControlLeft")
    (:right-control   "ControlRight")

    (:left-alt        "AltLeft")
    (:right-alt       "AltRight")

    (:left-super      "MetaLeft")
    (:right-super     "MetaRight")

    (:left-meta       "MetaLeft")
    (:right-meta      "MetaRight")

    ;; KeyboardEvent.code has no distinct Hyper code.
    (:left-hyper    nil)
    (:right-hyper   nil)

    (:kb-menu         "ContextMenu")

    (:keypad-0        "Numpad0")
    (:keypad-1        "Numpad1")
    (:keypad-2        "Numpad2")
    (:keypad-3        "Numpad3")
    (:keypad-4        "Numpad4")
    (:keypad-5        "Numpad5")
    (:keypad-6        "Numpad6")
    (:keypad-7        "Numpad7")
    (:keypad-8        "Numpad8")
    (:keypad-9        "Numpad9")
    (:keypad-decimal  "NumpadDecimal")
    (:keypad-divide   "NumpadDivide")
    (:keypad-multiply "NumpadMultiply")
    (:keypad-subtract "NumpadSubtract")
    (:keypad-add      "NumpadAdd")
    (:keypad-enter    "NumpadEnter")
    (:keypad-equal    "NumpadEqual")))

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
   (backend-window-should-close-p
    :reader backend-window-should-close-p :initform nil)
   (text-height :reader text-height :initform 10)
   (callback-handler :reader callback-handler)))

(register-backend 'backend/web +priority-native+)

;;TODO support this
;; (defgeneric callback-on-window-resize (handler width height))

(defmethod backend-init-window ((ctx backend/web) width height
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
           (callback-on-mouse-move callback-handler-instance
                                   (jscl/ffi:oget e "clientX")
                                   (jscl/ffi:oget e "clientY")))
         (on-mouse-down (e)
           (callback-on-mouse-down callback-handler-instance
                                   (let ((btn (jscl/ffi:oget e "button")))
                                     (cond ((= btn 0) :left)
                                           ((= btn 1) :middle)
                                           ((= btn 2) :right)))))
         (on-mouse-up (e)
           (callback-on-mouse-up callback-handler-instance
                                   (let ((btn (jscl/ffi:oget e "button")))
                                     (cond ((= btn 0) :left)
                                           ((= btn 1) :middle)
                                           ((= btn 2) :right)))))
         (on-key-down (e)
           (callback-on-key-down callback-handler-instance
                                 (js-key->lisp-key (jscl/ffi:oget e "code"))))
         (on-key-up (e)
           (callback-on-key-up callback-handler-instance
                               (js-key->lisp-key (jscl/ffi:oget e "code")))))

    (#j:document:addEventListener #j"mousemove" #'on-mouse-move)
    (#j:document:addEventListener #j"mousedown" #'on-mouse-down)
    (#j:document:addEventListener #j"mouseup" #'on-mouse-up)
    (#j:document:addEventListener #j"keydown" #'on-key-down)
    (#j:document:addEventListener #j"keyup" #'on-key-up))

  ;; Notify of canvas size
  (callback-on-window-resize callback-handler-instance width height)

  ctx)

(defmethod backend-close-window ((ctx backend/web))
  ((jscl/ffi:oget (slot-value ctx 'canvas-node) "remove")))

(defmethod backend-begin-drawing ((ctx backend/web))
  (error "Calling begin-drawing directly is not supported on the
           web backend. Use with-drawing instead."))

(defmethod backend-end-drawing ((ctx backend/web))
  (error "Calling end-drawing directly is not supported on the
           web backend. Use with-drawing instead."))

(defun color->jsstring (color)
  (jscl/ffi:jsstring (format nil "rgba(~a, ~a, ~a, ~a)" 
                             (color-r color)
                             (color-g color)
                             (color-b color)
                             (color-a color))))

(defmethod backend-draw-rectangle ((ctx backend/web) x y w h color)
  (with-slots (canvas-ctx) ctx
    (setf (jscl/ffi:oget canvas-ctx "fillStyle")
          (color->jsstring color))
    ((jscl/ffi:oget canvas-ctx "beginPath"))
    ((jscl/ffi:oget canvas-ctx "rect") x y w h)
    ((jscl/ffi:oget canvas-ctx "fill"))))

(defmethod backend-set-preferred-text-height ((ctx backend/web) text-height)
  (setf (slot-value ctx 'text-height) text-height))

(defmethod backend-get-text-height ((ctx backend/web))
  (slot-value ctx 'text-height))

(defmethod backend-measure-text-width ((ctx backend/web) text)
  (jscl/ffi:oget ((jscl/ffi:oget (canvas-ctx ctx) "measureText") text)
                 "width"))

(defmethod backend-draw-text ((ctx backend/web) x y color text)
  (setf (jscl/ffi:oget (slot-value ctx 'canvas-ctx) "fillStyle")
        (color->jsstring color))
  (setf (jscl/ffi:oget (slot-value ctx 'canvas-ctx) "font")
        (jscl/ffi:jsstring (format nil "~apx sans-serif"
                                   (slot-value ctx 'text-height))))
  ((jscl/ffi:oget (canvas-ctx ctx) "beginPath"))
  ((jscl/ffi:oget (canvas-ctx ctx) "fillText") (jscl/ffi:jsstring text)
   x (+ y (slot-value ctx 'text-height))))

(defmethod backend-draw-canvas ((ctx backend/web) x y canvas &optional tint)
  ;; TODO handle tint
  ((jscl/ffi:oget (slot-value ctx 'canvas-ctx) "drawImage") canvas x y))

(defmethod backend-create-canvas ((ctx backend/web) w h)
  (let* ((new-canvas-node (#j:document:createElement #j"canvas")))

    ;; todo store a reference to this canvas in our backend somewhere so we
    ;; can delete it later if we need
    (setf (jscl/ffi:oget new-canvas-node "width") w)
    (setf (jscl/ffi:oget new-canvas-node "height") h)
    (setf (jscl/ffi:oget new-canvas-node "style" "display") #j"none")
    (#j:document:body:append new-canvas-node)
    new-canvas-node))

(defmethod backend-destroy-canvas ((ctx backend/web) canvas)
  ((jscl/ffi:oget canvas "remove")))

(defmethod backend-check-for-input ((ctx backend/web))
  (slot-value ctx 'input-happened-p))

(defmethod backend-draw-rectangle-on-canvas ((ctx backend/web) canvas x y w h color)
  (let ((draw-ctx ((jscl/ffi:oget canvas "getContext") #j"2d")))
    (setf (jscl/ffi:oget draw-ctx "fillStyle")
          (color->jsstring color))
    ((jscl/ffi:oget draw-ctx "beginPath"))
    ((jscl/ffi:oget draw-ctx "rect") x y w h)
    ((jscl/ffi:oget draw-ctx "fill"))))

(defmethod backend-draw-text-on-canvas ((ctx backend/web) canvas x y color text)
  (let ((draw-ctx ((jscl/ffi:oget canvas "getContext") #j"2d")))
    ;; TODO set text height
    (setf (jscl/ffi:oget draw-ctx "font")
          (jscl/ffi:jsstring (format nil "~apx sans-serif"
                                     (slot-value ctx 'text-height))))
    (setf (jscl/ffi:oget draw-ctx "fillStyle") (color->jsstring color))
    ((jscl/ffi:oget draw-ctx "beginPath"))
    ((jscl/ffi:oget draw-ctx "fillText") (jscl/ffi:jsstring text)
     x (+ y (slot-value ctx 'text-height)))))


(defmethod backend-draw-canvas-on-canvas ((ctx backend/web) dst src
                                          dst-x dst-y
                                          src-x src-y
                                          src-w src-h &optional tint)
  (error "todo")
  ;; TODO
  )


