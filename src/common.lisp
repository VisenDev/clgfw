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
;;;; moved to %backend-web.lisp

(in-package #:clgfw)

;;; ==== BOOLEAN ====
(declaim (ftype (function (t) boolean) make-boolean))
(defun make-boolean (value)
  "Coerces a truthy or falsesy value to a boolean"
  (not (not value)))

;;; A BACKEND SHOULD CALL THESE FUNCTIONS WHEN THESE EVENTS OCCUR
;; (defun %callback-on-mouse-move    (handler x y))
;; (defun %callback-on-mouse-down    (handler mouse-button))
;; (defun %callback-on-mouse-up      (handler mouse-button))
;; (defun %callback-on-key-down      (handler key))
;; (defun %callback-on-key-up        (handler key))
;; (defun %callback-on-window-resize (handler width height))
;; (defun %callback-on-frame-begin   (handler width height))
;; (defun %callback-on-frame-end     (handler width height))

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
(defgeneric %backend-window-create (ctx width height title
                                    %callback-handler-instance))
(defgeneric %backend-window-run (ctx draw-function-callback))
(defgeneric %backend-request-quit (ctx))

(defgeneric %backend-clipboard-get (ctx))
(defgeneric %backend-clipboard-set (ctx string))

(defgeneric %backend-scissor-begin (ctx x y w h))
(defgeneric %backend-scissor-end (ctx))

(defgeneric %backend-set-preferred-text-height (ctx text-height))
(defgeneric %backend-get-text-height (ctx))
(defgeneric %backend-measure-text-width (ctx text))

(defgeneric %backend-draw-rectangle (ctx x y w h color
                                    &key angle origin-x origin-y target))
(defgeneric %backend-draw-text (ctx text x y color
                               &key angle origin-x origin-y target))
(defgeneric %backend-draw-canvas (ctx canvas dst-x dst-y
                                  &key angle origin-x origin-y tint target
                                    dst-w dst-h
                                    src-x src-y src-w src-h))

(defgeneric %backend-canvas-create             (ctx w h))
(defgeneric %backend-canvas-destroy            (ctx canvas))

;; NEW APIS THAT I HAVEN'T IMPLEMENTED YET
;; TODO: add frontend interfaces for these
;; TODO: implement these in the web backend for now
(defgeneric %backend-blit                  (ctx x y w h pixels &key target))
(defgeneric %backend-read-pixels           (ctx x y w h &key target))
(defgeneric %backend-gamepads-list         (ctx))
(defgeneric %backend-gamepad-name          (ctx gamepad))
(defgeneric %backend-gamepad-button-down-p (ctx gamepad button))
(defgeneric %backend-gamepad-axis-read     (ctx gamepad axis))

(deftype canvas () 't)
(deftype redraw-frequency-type () `(member :target-fps :on-input))

(defstruct (window-state (:conc-name ws-))
  (backend nil :type t)
  (keyboard-state (make-hash-table :test 'eq :size 256) :type hash-table)
  (window-width 0 :type fixnum)
  (window-height 0 :type fixnum)
  (old-window-width 0 :type fixnum) ;; for checking if the window width has changed
  (old-window-height 0 :type fixnum) ;; for checking if the window height has changed
  (mouse-x 0 :type fixnum)
  (mouse-y 0 :type fixnum)
  (mouse-button-states (make-hash-table :test 'eq)
   :type hash-table)
  (pressed-keys (make-array 256 :element-type '(or key null)
                                :fill-pointer 0 :initial-element nil)
   :type (vector symbol 256)) ;; A vector of the keys pressed this frame
  (released-keys (make-array 256 :element-type '(or key null)
                                 :fill-pointer 0 :initial-element nil)
   :type (vector symbol 256))  ;; A vector of the keys released this frame
  (pressed-mouse-buttons (make-array 3 :element-type '(or mouse-button null)
                                 :fill-pointer 0 :initial-element nil)
   :type (vector (or mouse-button null) 3))
  (released-mouse-buttons (make-array 3 :element-type '(or mouse-button null)
                                        :fill-pointer 0 :initial-element nil)
   :type (vector (or mouse-button null) 3))
  (target-fps 60 :type real)
  (fps-history (make-array 60 :adjustable t
                              :fill-pointer 0)
   :type (vector real *))
  (redraw-frequency :on-input :type redraw-frequency-type)
  (last-frame-timestamp (timestamp-get) :type integer)
  (current-frame-timestamp (timestamp-get) :type integer)
  (delta-time-seconds 0 :type number)
  (input-happened-p t :type boolean))


(declaim (ftype (function (window-state number number) t) %callback-on-mouse-move))
(defun %callback-on-mouse-move (handler x y)
  (unless (and (= (ws-mouse-x handler) x)
               (= (ws-mouse-y handler) y))
    (setf (ws-input-happened-p handler) t)
    (setf (ws-mouse-x handler) x)
    (setf (ws-mouse-y handler) y)))

(declaim (ftype (function (window-state mouse-button) t) %callback-on-mouse-down))
(defun %callback-on-mouse-down (handler mouse-button)
  (setf (ws-input-happened-p handler) t)
  (vector-push mouse-button (ws-pressed-mouse-buttons handler))
  (setf (gethash mouse-button (ws-mouse-button-states handler)) t))

(declaim (ftype (function (window-state mouse-button) t) %callback-on-mouse-up))
(defun %callback-on-mouse-up (handler mouse-button)
  (setf (ws-input-happened-p handler) t)
  (vector-push mouse-button (ws-released-mouse-buttons handler))
  (setf (gethash mouse-button (ws-mouse-button-states handler)) nil))

(declaim (ftype (function (window-state key) t) %callback-on-key-down))
(defun %callback-on-key-down (handler key)
  (setf (ws-input-happened-p handler) t)
  (vector-push key (ws-pressed-keys handler))
  (setf (gethash key (ws-keyboard-state handler)) t))

(declaim (ftype (function (window-state key) t) %callback-on-key-up))
(defun %callback-on-key-up  (handler key)
  (setf (ws-input-happened-p handler) t)
  (vector-push key (ws-released-keys handler))
  (setf (gethash key (ws-keyboard-state handler)) nil))

(declaim (ftype (function (window-state number number) t)
                %callback-on-window-resize))
(defun %callback-on-window-resize (handler width height)
  (unless (and (= (ws-window-width handler) width)
               (= (ws-window-height handler) height))
    (setf (ws-input-happened-p handler) t)
    (setf (ws-window-width handler) width)
    (setf (ws-window-height handler) height)))

(declaim (ftype (function (window-state) t) %record-timestamp))
(defun %record-timestamp (window-state)
  (setf (ws-last-frame-timestamp window-state)
        (ws-current-frame-timestamp window-state))
  (setf (ws-current-frame-timestamp window-state) (timestamp-get))
  (setf (ws-delta-time-seconds window-state)
        (timestamp-difference-seconds 
         (ws-last-frame-timestamp window-state)
         (ws-current-frame-timestamp window-state))))

(declaim (ftype (function (window-state) t) %callback-on-frame-begin))
(defun %callback-on-frame-begin (handler)
  (%record-timestamp handler))

(declaim (ftype (function (window-state) t) %callback-on-frame-end))
(defun %callback-on-frame-end (handler)
  (setf (fill-pointer (ws-pressed-keys handler)) 0)
  (setf (fill-pointer (ws-released-keys handler)) 0)
  (setf (fill-pointer (ws-pressed-mouse-buttons handler)) 0)
  (setf (fill-pointer (ws-released-mouse-buttons handler)) 0)
  (setf (ws-old-window-width handler) (ws-window-width handler))
  (setf (ws-old-window-height handler) (ws-window-height handler))

  #|(ecase redraw-frequency
      (:target-fps
       (let ((remaining (get-remaining-seconds-in-frame handler)))
         (when (plusp remaining)
           #-jscl(sleep remaining))))
      (:on-input
       (loop :until input-happened-p
             :do #-jscl(sleep 0.001)
                 (%backend-check-for-input backend)
             :finally (setf input-happened-p nil)))) |#
  )


(defun get-prioritized-backends ()
  "Returns a list of available backends sorted by priority"
  (sort (let ((backends nil))
          (maphash (lambda (key value)
                     (declare (ignore key))
                     (push value backends))
                   *backends*)
          backends)
        (lambda (lhs rhs)
          (> (getf lhs :priority)
             (getf rhs :priority)))))

;;; ==== PUBLIC INTERFACE ====
(declaim (ftype (function (integer integer string) t) window-create))
(defun window-create (width height title)
  "Attempts to initialize a window on your platform"
  (let ((prioritized-backends (get-prioritized-backends))
        (window (make-window-state)))
    (dolist (%backend-info prioritized-backends)
      (let* ((instance (make-instance (getf %backend-info :class-name))))
        (let ((backend (handler-case
                           (%backend-window-create instance width height title
                                                   window)
                         #-jscl
                         (error (e)
                           (format t "~a" e)
                           nil))))
          (when backend
            (setf (ws-backend window) backend)
            (return-from window-create window))))))
  (error "No appropriate backend found :("))

(declaim (ftype (function (window-state) fixnum) get-mouse-x))
(defun get-mouse-x (window-state)
  (ws-mouse-x window-state))

(declaim (ftype (function (window-state) fixnum) get-mouse-y))
(defun get-mouse-y (window-state)
  (ws-mouse-y window-state))

(declaim (ftype (function (window-state) fixnum) get-window-width))
(defun get-window-width (window-state)
  (ws-window-width window-state))

(declaim (ftype (function (window-state) fixnum) get-window-height))
(defun get-window-height (window-state)
  (ws-window-height window-state))

(declaim (ftype (function (window-state mouse-button) boolean) is-mouse-button-down))
(defun is-mouse-button-down (window-state button)
  (gethash button (ws-mouse-button-states window-state)))

(declaim (ftype (function (window-state mouse-button) boolean) is-mouse-button-up))
(defun is-mouse-button-up (window-state button)
  (not (gethash button (ws-mouse-button-states window-state))))

(declaim (ftype (function (window-state mouse-button) boolean)
                is-mouse-button-pressed))
(defun is-mouse-button-pressed (window-state button)
  (make-boolean (find button (ws-pressed-mouse-buttons window-state))))

(declaim (ftype (function (window-state mouse-button) boolean)
                is-mouse-button-released))
(defun is-mouse-button-released (window-state button)
  (make-boolean (find button (ws-released-mouse-buttons window-state))))

(declaim (ftype (function (window-state key) boolean) is-key-down))
(defun is-key-down (window-state key)
  (make-boolean (gethash key (ws-keyboard-state window-state) nil)))

(declaim (ftype (function (window-state key) boolean) is-key-up))
(defun is-key-up (window-state key)
  (make-boolean (not (gethash key (ws-keyboard-state window-state) nil))))

(declaim (ftype (function (window-state key) boolean) is-key-pressed))
(defun is-key-pressed (window-state key)
  (make-boolean (find key (ws-pressed-keys window-state))))

(declaim (ftype (function (window-state key) boolean) is-key-released))
(defun is-key-released (window-state key)
  (make-boolean (find key (ws-released-keys window-state))))

(declaim (ftype (function (window-state) real) get-fps))
(defun get-fps (window-state)
  (or (ignore-errors (/ 1 (ws-delta-time-seconds window-state))) 0))

(declaim (ftype (function (window-state) real) get-delta-time))
(defun get-delta-time (window-state)
  "Returns delta time in seconds"
  (ws-delta-time-seconds window-state))

(declaim (ftype (function (window-state) string) get-fps-string))
(defun get-fps-string (window-state)
  (format nil "~a FPS" (floor (get-fps window-state))))

(declaim (ftype (function (window-state) real) get-seconds-passed-in-frame))
(defun get-seconds-passed-in-frame (window-state)
  (timestamp-difference-seconds (ws-current-frame-timestamp window-state)
                                (timestamp-get)))

(declaim (ftype (function (window-state) real) get-target-seconds-per-frame))
(defun get-target-seconds-per-frame (window-state)
  (/ 1 (ws-target-fps window-state)))

(declaim (ftype (function (window-state) real) get-remaining-seconds-in-frame))
(defun get-remaining-seconds-in-frame (window-state)
  (- (get-target-seconds-per-frame window-state)
     (get-seconds-passed-in-frame window-state)))

(declaim
 (ftype
  (function
   (window-state
    number number number number color &key (:angle number) (:origin-x number)
    (:origin-y number) (:target canvas))
   t)
  draw-rectangle))
(defun draw-rectangle (ctx x y w h color &key (angle 0) (origin-x 0)
                                           (origin-y 0) target)
  (%backend-draw-rectangle (ws-backend ctx)
                           x y w h color :angle angle
                           :origin-x origin-x
                           :origin-y origin-y
                           :target target))


(declaim
 (ftype
  (function
   (window-state string number number color &key (:angle number)
                 (:origin-x number) (:origin-y number) (:target canvas))
   t)
  draw-text))
(defun draw-text (ctx text x y color &key (angle 0) (origin-x 0) (origin-y 0) target)
  (%backend-draw-text (ws-backend ctx) text x y color :angle angle :origin-x origin-x
                                         :origin-y origin-y :target target))

(declaim
 (ftype
  (function
   (window-state canvas number number &key (:angle number) (:origin-x number)
                 (:origin-y number) (:tint color) (:target t) (:dst-w number)
                 (:dst-h number) (:src-x number) (:src-y number) (:src-w number)
                 (:src-h number))
   t)
  draw-canvas))
(defun draw-canvas (ctx canvas dst-x dst-y
                    &key
                      (angle 0) (origin-x 0) (origin-y 0) tint target
                      dst-w dst-h
                      (src-x 0) (src-y 0) src-w src-h)
  (%backend-draw-canvas (ws-backend ctx) canvas dst-x dst-y
                        :angle angle :origin-x origin-x
                        :origin-y origin-y :tint tint :target target :dst-w dst-w
                        :dst-h dst-h :src-x src-x :src-y src-y :src-w src-w
                        :src-h src-h))


;;; CANVAS
(declaim (ftype (function (window-state number number) t)))
(defun create-canvas (window-state width height)
  (%backend-canvas-create (slot-value window-state 'backend)
                         (floor width) (floor height)))

(declaim (ftype (function (window-state t) t) destroy-canvas))
(defun destroy-canvas (window-state canvas)
  (%backend-canvas-destroy (slot-value window-state 'backend)
                          canvas))

;;; TEXT HEIGHT
(declaim (ftype (function (window-state number) t) set-preferred-text-height))
(defun set-preferred-text-height (window-state text-height)
  "Requests that the backend draws text at the given text-height. Might not
   always work because certain backends (ie clx) cannot draw arbitrary text
   sizes. Always use the text measuring functions to check the real size that
   text will be rendered at."
  (%backend-set-preferred-text-height (ws-backend window-state)
                                     (round text-height)))


;;; REDRAW AND FPS
(declaim (ftype (function (window-state redraw-frequency-type &optional number) t)
                set-redraw-frequency))
(defun set-redraw-frequency (window-state redraw-frequency-type
                             &optional frames-per-second)
  (ecase redraw-frequency-type
    (:target-fps
     (assert frames-per-second () "expected target frames-per-second")
     (setf (ws-target-fps window-state) frames-per-second)
     (setf (ws-redraw-frequency window-state) :target-fps))
    (:on-input
     (assert (not frames-per-second))
     (setf (ws-redraw-frequency window-state) :on-input))))

(defun window-run (window-state draw-function)
  (%backend-window-run (ws-backend window-state) draw-function ))

(defun request-quit (window-state)
  (%backend-request-quit (ws-backend window-state)))

