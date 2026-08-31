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

;;;; TODO: refactor the api so that the jscl
;;;; specific code that had to be added here can be
;;;; moved to backend-web.lisp

(in-package #:clgfw)

;;; ==== BOOLEAN ====
(declaim (ftype (function (t) boolean) make-boolean))
(defun make-boolean (value)
  "Coerces a truthy or falsesy value to a boolean"
  (not (not value)))

;;; A BACKEND SHOULD CALL THESE FUNCTIONS WHEN THESE EVENTS OCCUR
(defgeneric callback-on-mouse-move    (handler x y))
(defgeneric callback-on-mouse-down    (handler mouse-button))
(defgeneric callback-on-mouse-up      (handler mouse-button))
(defgeneric callback-on-key-down      (handler key))
(defgeneric callback-on-key-up        (handler key))
(defgeneric callback-on-window-resize (handler width height))
(defgeneric callback-all-keys-up      (handler))

;;; USE THESE FUNCTIONS AND CONSTANTS TO REGISTER YOUR NEW BACKEND
(defvar *backends* (make-hash-table))
(defconstant +priority-native+ 99
  "For backends that are native or built-in to the lisp implementation")
(defconstant +priority-primary+ 66)
(defconstant +priority-secondary+ 33)
(defconstant +priority-last+ 0
  "For backends that should only be used as a last resort")
(defun unregister-all-backends ()
  (clrhash *backends*))
(defun register-backend (class-name priority &optional testing)
  "Use this to tell clgfw about a new backend that is available,
   the predefined +priority-foo+ constants can be used to specify
   which backend have priority. The testing parameter is used to
   tell clgfw that the backend is currently being developed and
   experimented with so it should temporarily take priority"
  (setf (gethash class-name *backends*)
        (list :priority (if testing most-positive-fixnum
                            priority)
              :class-name class-name)))

;;; A BACKEND SHOULD BE A CLASS THAT IMPLEMENT THESE FUNCTIONS
(defgeneric backend-init-window               (ctx width height title
                                               callback-handler-instance))
(defgeneric backend-run                       (ctx main-function-callback))
;; (defgeneric backend-close-window              (ctx))
;; (defgeneric backend-window-should-close-p     (ctx))
;; (defgeneric backend-begin-drawing             (ctx))
;; (defgeneric backend-end-drawing               (ctx))
(defgeneric backend-draw-rectangle            (ctx x y w h color
                                               &key angle-degrees))
(defgeneric backend-set-preferred-text-height (ctx text-height))
(defgeneric backend-get-text-height           (ctx))
(defgeneric backend-measure-text-width        (ctx text))
(defgeneric backend-draw-text                 (ctx x y color text))
(defgeneric backend-draw-canvas               (ctx x y canvas &optional tint))
(defgeneric backend-create-canvas             (ctx w h))
(defgeneric backend-destroy-canvas            (ctx canvas))
(defgeneric backend-check-for-input           (ctx))
(defgeneric backend-draw-rectangle-on-canvas  (ctx canvas x y w h color))
(defgeneric backend-draw-text-on-canvas       (ctx canvas x y color text))
(defgeneric backend-draw-canvas-on-canvas     (ctx dst dst-x dst-y src
                                               &optional tint))
(deftype redraw-frequency-type () `(member :target-fps :on-input))

(defclass window-state ()
  ((backend :accessor backend)
   (keyboard-state :accessor keyboard-state :initform
                   (make-hash-table :test 'eq :size 256))
   (window-width :accessor window-width :initform 0)
   (window-height :accessor window-height :initform 0)
   (old-window-width :accessor old-window-width :initform 0
                     :documentation "Used to check if the current window width has changed")
   (old-window-height :accessor old-window-height :initform 0
                      :documentation "Used to check if the current window height has changed")
   (mouse-x :initform 0 :accessor mouse-x :type fixnum)
   (mouse-y :initform 0 :accessor mouse-y :type fixnum)
   (mouse-button-states :initform (make-hash-table :test 'eq)
                        :accessor mouse-button-states)
   (pressed-keys
    :accessor pressed-keys
    :initform (make-array 256 :element-type 'symbol
                              :fill-pointer 0 :initial-element nil)
    :documentation "A vector of all the keys which have been pressed this frame")
   (released-keys
    :accessor released-keys
    :initform (make-array 256 :element-type 'symbol
                              :fill-pointer 0 :initial-element nil)
    :documentation "A vector of all the keys which have been released this frame")
   (pressed-mouse-buttons
    :accessor pressed-mouse-buttons
    :initform (make-array 3 :element-type 'symbol
                            :fill-pointer 0 :initial-element nil))
   (released-mouse-buttons
    :accessor released-mouse-buttons
    :initform (make-array 3 :element-type 'symbol
                            :fill-pointer 0 :initial-element nil))
   (target-fps :accessor target-fps :initform 60)
   (fps-history :accessor fps-history :initform (make-array 60 :adjustable t
                                                               :fill-pointer 0))
   (redraw-frequency :accessor redraw-frequency
                     :initform :on-input :type redraw-frequency-type)
   (last-frame-timestamp :accessor last-frame-timestamp :initform (timestamp-get))
   (current-frame-timestamp :accessor current-frame-timestamp
                            :initform (timestamp-get))
   (delta-time-seconds :accessor delta-time-seconds :initform 0)
   (input-happened-p :accessor input-happened-p :initform t)
   (draw-on-canvas? :accessor draw-on-canvas? :initform nil
                    :documentation "When non-nil, draw commands should apply to this
                                    canvas instead of the window")))



(defmethod callback-on-mouse-move ((handler window-state) x y)
  (with-slots (mouse-x mouse-y input-happened-p) handler
      (unless (and (= mouse-x x)
                   (= mouse-y y))
        (setf input-happened-p t)
        (setf mouse-x x)
        (setf mouse-y y))))

(defmethod callback-on-mouse-down ((handler window-state) mouse-button)
  (with-slots
        (input-happened-p pressed-mouse-buttons mouse-button-states) handler
      (setf input-happened-p t)
    (vector-push mouse-button pressed-mouse-buttons)
    (setf (gethash mouse-button mouse-button-states) t)))

(defmethod callback-on-mouse-up ((handler window-state) mouse-button)
  (setf (slot-value handler 'input-happened-p) t)
  (vector-push mouse-button (slot-value handler 'released-mouse-buttons))
  (setf (gethash mouse-button (slot-value handler 'mouse-button-states)) nil))

(defmethod callback-on-key-down ((handler window-state) key)
  #-jscl(declare (type key key))
  (setf (slot-value handler 'input-happened-p) t)
  (vector-push key (slot-value handler 'pressed-keys))
  (setf (gethash key (slot-value handler 'keyboard-state)) t))

(defmethod callback-on-key-up  ((handler window-state) key)
  #-jscl(declare (type key key))
  (setf (slot-value handler 'input-happened-p) t)
  (vector-push key (slot-value handler 'released-keys))
  (setf (gethash key (slot-value handler 'keyboard-state)) nil))

(defmethod callback-all-keys-up ((handler window-state))
  (setf (slot-value handler 'input-happened-p) t)
  (loop :for key :across (slot-value handler 'pressed-keys)
        :do (callback-on-key-up handler key)
        :finally (setf (fill-pointer (slot-value handler 'pressed-keys)) 0)))

(defmethod callback-on-window-resize ((handler window-state) width height)
  (with-slots (window-width window-height input-happened-p) handler
    (unless (and (= window-width width)
                 (= window-height height))
      (setf input-happened-p t)
      (setf window-width width)
      (setf window-height height))))

;;; ==== PUBLIC INTERFACE ====
(defun init-window (width height title)
  "Attempts to initialize a window on your platform"
  (let ((prioritized-backends (sort (let ((backends nil))
                                      (maphash (lambda (key value)
                                                 (push value backends))
                                               *backends*)
                                      backends)
                                    (lambda (lhs rhs)
                                      (>
                                       (getf lhs :priority)
                                       (getf rhs :priority)))))
        (window (make-instance 'window-state)))
    ;; (error "Backends: ~a" prioritized-backends)
    ;; (#j:console:log (jscl/ffi:jsstring
    ;;                  (format nil "Backends: ~a" prioritized-backends)))
    (dolist (backend-info prioritized-backends)
      (let* ((instance (make-instance (getf backend-info :class-name))))
        (let ((backend (handler-case
                           (backend-init-window instance width height title window)
                         ;; NOTE: Don't handle the error on jscl because the format
                         ;; output doesn't get printed to the console so we will
                         ;; have no feedback on what the actual error was
                         #-jscl
                         (error (e)
                           (format t "Error initializing using ~a~%~a~%" instance e)
                           nil)
                         )))
          (when backend
            (setf (slot-value window 'backend) backend)
            (return-from init-window window))))))
  (error "No appropriate backend found :("))

(declaim (ftype (function (window-state) t) close-window))
(defun close-window (window-state)
  (backend-close-window (slot-value window-state 'backend)))

(declaim (ftype (function (window-state) fixnum) get-mouse-x))
(defun get-mouse-x (window-state)
  (slot-value window-state 'mouse-x))

(declaim (ftype (function (window-state) fixnum) get-mouse-y))
(defun get-mouse-y (window-state)
  (slot-value window-state 'mouse-y))

(declaim (ftype (function (window-state) fixnum) get-window-width))
(defun get-window-width (window-state)
  (slot-value window-state 'window-width))

(declaim (ftype (function (window-state) fixnum) get-window-height))
(defun get-window-height (window-state)
  (slot-value window-state 'window-height))

(declaim (ftype (function (window-state mouse-button) boolean) is-mouse-button-down))
(defun is-mouse-button-down (window-state button)
  (gethash button (slot-value window-state 'mouse-button-states)))

(declaim (ftype (function (window-state mouse-button) boolean) is-mouse-button-up))
(defun is-mouse-button-up (window-state button)
  (not (gethash button (slot-value window-state 'mouse-button-states))))

(declaim (ftype (function (window-state mouse-button) boolean)
                is-mouse-button-pressed))
(defun is-mouse-button-pressed (window-state button)
  (make-boolean (find button (slot-value window-state 'pressed-mouse-buttons))))

(declaim (ftype (function (window-state mouse-button) boolean)
                is-mouse-button-released))
(defun is-mouse-button-released (window-state button)
  (make-boolean (find button (slot-value window-state 'released-mouse-buttons))))

(declaim (ftype (function (window-state key) boolean) is-key-down))
(defun is-key-down (window-state key)
  (make-boolean (gethash key (slot-value window-state 'keyboard-state) nil)))

(declaim (ftype (function (window-state key) boolean) is-key-up))
(defun is-key-up (window-state key)
  (make-boolean (not (gethash key (slot-value window-state 'keyboard-state) nil))))

(declaim (ftype (function (window-state key) boolean) is-key-pressed))
(defun is-key-pressed (window-state key)
  (make-boolean (find key (slot-value window-state 'pressed-keys))))

(declaim (ftype (function (window-state key) boolean) is-key-released))
(defun is-key-released (window-state key)
  (make-boolean (find key (slot-value window-state 'released-keys))))

(declaim (ftype (function (window-state) t) %record-timestamp))
(defun %record-timestamp (window-state)
  (with-slots (backend last-frame-timestamp current-frame-timestamp
               delta-time-seconds)
      window-state
    
    (setf last-frame-timestamp current-frame-timestamp)
    (setf current-frame-timestamp (timestamp-get))
    (setf delta-time-seconds
          (timestamp-difference-seconds 
           last-frame-timestamp
           current-frame-timestamp))))

(declaim (ftype (function (window-state) t) begin-drawing))
(defun begin-drawing (window-state)
  (%record-timestamp window-state)
  (backend-begin-drawing (slot-value window-state 'backend)))

(defun get-fps (window-state)
  (or
   (ignore-errors (/ 1 (delta-time-seconds window-state)))
   0))

(defun get-delta-time (window-state)
  "Returns delta time in seconds"
  (delta-time-seconds window-state))

(defun get-fps-string (window-state)
  (format nil "~a FPS" (floor (get-fps window-state))))

(defun get-seconds-passed-in-frame (window-state)
  (with-slots (current-frame-timestamp) window-state
    (timestamp-difference-seconds current-frame-timestamp (timestamp-get))))

(defun get-target-seconds-per-frame (window-state)
  (with-slots (target-fps) window-state
    (/ 1 target-fps)))

(defun get-remaining-seconds-in-frame (window-state)
  (- (get-target-seconds-per-frame window-state)
     (get-seconds-passed-in-frame window-state)))

(declaim (ftype (function (window-state) t) end-drawing))
(defun end-drawing (window-state)
  (with-slots (pressed-keys released-keys pressed-mouse-buttons
               released-mouse-buttons old-window-width old-window-height
               window-width window-height redraw-frequency backend
               input-happened-p)
      window-state
    (backend-end-drawing backend)
    (setf (fill-pointer pressed-keys) 0)
    (setf (fill-pointer released-keys) 0)
    (setf (fill-pointer pressed-mouse-buttons) 0)
    (setf (fill-pointer released-mouse-buttons) 0)
    (setf old-window-width window-width)
    (setf old-window-height window-height)

    (ecase redraw-frequency
      (:target-fps
       (let ((remaining (get-remaining-seconds-in-frame window-state)))
         (when (plusp remaining)
           #-jscl(sleep remaining))))
      (:on-input
       (loop :until input-happened-p
             :do #-jscl(sleep 0.001)
                 (backend-check-for-input backend)
             :finally (setf input-happened-p nil))))))

(declaim (ftype (function (window-state) boolean) window-should-close-p]))
(defun window-should-close-p (window-state)
  (backend-window-should-close-p (slot-value window-state 'backend)))

(declaim (ftype (function (window-state) boolean) window-should-keep-running-p))
(defun window-should-keep-running-p (window-state)
  (not (backend-window-should-close-p (slot-value window-state 'backend))))

(declaim (ftype (function (window-state number number number number color) t) draw-rectangle))
(defun draw-rectangle (window-state x y w h color)
  (with-slots (backend draw-on-canvas?) window-state
    (if draw-on-canvas?
        (backend-draw-rectangle-on-canvas backend draw-on-canvas? x y w h color)
        (backend-draw-rectangle backend x y w h color))))

(declaim (ftype (function (window-state number number color string) t) draw-text))
(defun draw-text (window-state x y color text)
  (with-slots (backend draw-on-canvas?) window-state
    (if draw-on-canvas?
        (backend-draw-text-on-canvas backend draw-on-canvas? x y color text)
        (backend-draw-text backend x y color text))))

(defun draw-fps (window-state x y &optional (color (make-color 200 200 200)))
  (draw-text window-state x y color (get-fps-string window-state)))

;; (defun draw-fps-graph (window-state x y &key
;;                          (history-length 60)
;;                          (line-width 2))

;;   (error "TODO")
;;   )

(declaim (ftype (function (window-state number number t &optional color) t)
                draw-canvas))
(defun draw-canvas (window-state x y canvas &optional tint)
  (with-slots (backend draw-on-canvas?) window-state
    (if draw-on-canvas?
        (backend-draw-canvas-on-canvas backend draw-on-canvas? x y canvas tint)
        (backend-draw-canvas backend x y canvas tint))))


;;; CANVAS
(declaim (ftype (function (window-state number number) t)))
(defun create-canvas (window-state width height)
  (backend-create-canvas (slot-value window-state 'backend)
                         (floor width) (floor height)))

(declaim (ftype (function (window-state t) t) destroy-canvas))
(defun destroy-canvas (window-state canvas)
  (backend-destroy-canvas (slot-value window-state 'backend)
                          canvas))

(defmacro with-canvas (varname (window-state width height) &body body)
  `(let ((,varname (create-canvas ,window-state ,width ,height)))
     (unwind-protect
          (progn ,@body)
       (destroy-canvas ,window-state ,varname))))

(defmacro with-canvases (window-state (&rest |(varname width height)|) &body body)
  (let ((forms |(varname width height)|))
    `(let ,(loop :for form :in forms
                 :for varname = (first form)
                 :for width = (second form)
                 :for height = (third form)
                 :collect `(,varname (create-canvas ,window-state ,width ,height)))
       (unwind-protect
            (progn ,@body)
         ,@(mapcar (lambda (form) `(destroy-canvas ,window-state ,(first form)))
                   forms)))))

(defun begin-drawing-on-canvas (window-state canvas)
  (with-slots (draw-on-canvas?) window-state
    (assert (null draw-on-canvas?))
    (setf draw-on-canvas? canvas)))

(defun end-drawing-on-canvas (window-state canvas)
  (with-slots (draw-on-canvas?) window-state
    (assert (eq draw-on-canvas? canvas))
    (setf draw-on-canvas? nil)))

(defmacro with-drawing-on-canvas ((window-state canvas) &body body)
  `(unwind-protect
       (progn
         (begin-drawing-on-canvas ,window-state ,canvas)
         ,@body)
    (end-drawing-on-canvas ,window-state ,canvas)))


;;; TEXT HEIGHT
(declaim (ftype (function (window-state number) t) set-preferred-text-height))
(defun set-preferred-text-height (window-state text-height)
  "Requests that the backend draws text at the given text-height. Might not always work because
   certain backends (ie clx) cannot draw arbitrary text sizes. Always use the text measuring
   functions to check the real size that text will be rendered at."
  (backend-set-preferred-text-height (slot-value window-state 'backend)
                                     (round text-height)))


;;; REDRAW AND FPS
(declaim (ftype (function (window-state redraw-frequency-type &optional number) t)
                set-redraw-frequency))
(defun set-redraw-frequency (window-state redraw-frequency-type
                             &optional frames-per-second)
  (ecase redraw-frequency-type
    (:target-fps
     (assert frames-per-second () "expected target frames-per-second")
     (setf (slot-value window-state 'target-fps) frames-per-second)
     (setf (slot-value window-state 'redraw-frequency) :target-fps))
    (:on-input
     (assert (not frames-per-second))
     (setf (slot-value window-state 'redraw-frequency) :on-input))))

;;;; NOTE: because of the browser's syncronous event loop
;;;; we need to special case with-drawing and while-running.
;;;; Otherwise we end up with an infinite loop that never
;;;; lets the browser render stuff and process events.


#-jscl
(defmacro with-window (name (width height title) &body body)
  `(let ((,name (init-window ,width ,height ,title)))
     (unwind-protect
          (progn ,@body)
       (close-window ,name))))

#+jscl
(defmacro with-window (name (width height title) &body body)
  `(let ((,name (init-window ,width ,height ,title)))
     (progn ,@body)))

#-jscl
(defmacro with-drawing (state &body body)
  `(progn
     (begin-drawing ,state)
     (unwind-protect (progn ,@body)
       (end-drawing ,state))))

#+jscl
(defmacro with-drawing (state &body body)
  `(progn ,@body))

#-jscl
(defmacro while-running (state &body body)
  `(loop :while (window-should-keep-running-p ,state)
         :do ,@body))

#+jscl
(defmacro while-running (state &body body)
  (let ((callback (gensym)))
    `(labels ((,callback (timestamp)
                (%record-timestamp ,state)
                (progn
                  ,@body)
                (if (window-should-keep-running-p ,state)
                    (#j:window:requestAnimationFrame #',callback)
                    (close-window ,state))))
       (#j:window:requestAnimationFrame #',callback))))


