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

;;; ==== BOOLEAN ====
(declaim (ftype (function (t) boolean) make-boolean))
(defun make-boolean (value)
  "Coerces a truthy or falsesy value to a boolean"
  (not (not value)))

;;; ==== COLORS ====
(deftype color () '(integer 0 #xffffffff))

(defmacro define-color-byte-accessor (name offset)
  `(progn
     (declaim (ftype (function (color) fixnum) ,name))
     (defun ,name (color)
       (declare (type color color)
                (optimize (speed 3) (safety 3) (debug 3)))
       (the fixnum (ldb (byte 8 ,offset) color)))
     (define-setf-expander ,name (color &environment env)
       (get-setf-expansion `(ldb (byte 8 ,,offset) ,color) env))))

(define-color-byte-accessor color-r 24)
(define-color-byte-accessor color-g 16)
(define-color-byte-accessor color-b 8)
(define-color-byte-accessor color-a 0)

(declaim (ftype (function (&optional fixnum fixnum fixnum fixnum) color) make-color))
(defun make-color (&optional (r 0) (g 0) (b 0) (a #xff))
  (declare (optimize (speed 3) (safety 3) (debug 3))
           (type color r g b a))
  (let* ((result (the color r))
         (result (the color (ash result 8)))
         (result (the color (logior result g)))
         (result (the color (ash result 8)))
         (result (the color (logior result b)))
         (result (the color (ash result 8)))
         (result (the color (logior result a))))
    (the color result)))

(declaim (ftype (function (fixnum) fixnum) clamp-u8))
(defun clamp-u8 (number)
  (declare (optimize (speed 3) (safety 0))
           (type fixnum number))
  (the fixnum
       (cond
         ((< number 0) 0)
         ((> number 255) 255)
         (t number))))

(defun color-premultiply-alpha (color)
  (let ((a (/ (color-a color) 255)))
    (make-color (floor (* a (color-r color)))
                (floor (* a (color-g color)))
                (floor (* a (color-b color)))
                (floor (* a (color-a color))))))

;; (declaim (ftype (function (color color) color) color-blend))
(defun color-blend (base applied-color)
  ;; (declare (optimize (speed 3)
  ;;                    (safety 3)
  ;;                    (debug 3))
  ;;          (type color base applied-color))
  (let* ((applied-color-a (the color (color-a applied-color)))
         (base-a (the color (color-a base)))
         (inverse-applied-a (the color (- 255 applied-color-a)))
         (out-a (the color (+ applied-color-a (truncate (+ (the color (* base-a inverse-applied-a)) 127) 255)))))
    (when (zerop out-a)
      (return-from color-blend
        (make-color 0 0 0 0)))

    (let* ((r (the color (truncate
                          (+ (the color (* (color-r applied-color) applied-color-a))
                             (truncate (the color (* (color-r base) base-a inverse-applied-a)) 255))
                          out-a)))
           (g (the color (truncate
                          (+ (the color (* (color-g applied-color) applied-color-a))
                             (truncate (the color (* (color-g base) base-a inverse-applied-a)) 255))
                          out-a)))
           (b (the color (truncate
                          (+ (the color (* (color-b applied-color) applied-color-a))
                             (truncate (the color (* (color-b base) base-a inverse-applied-a)) 255))
                          out-a))))
      (make-color
       (clamp-u8 r)
       (clamp-u8 g)
       (clamp-u8 b)
       (clamp-u8 out-a)))))

(defun color-invisible-p (color)
  (declare (type color color))
  (= 0 (color-a color)))

(defun color-opaque-p (color)
  (declare (type color color)
           (optimize (speed 3)))
  (= 255 (color-a color)))

;;; KEY
(deftype key ()
  '(member
    :quote :comma :minus :period :slash
    :zero :one :two :three :four :five :six :seven :eight :nine
    :semicolon :equal
    :a :b :c :d :e :f :g :h :i :j :k :l :m
    :n :o :p :q :r :s :t :u :v :w :x :y :z
    :left-bracket :backslash :right-bracket :backtick
    :space :escape :enter :tab :backspace :insert :delete
    :right :left :down :up :page-up :page-down :home
    :end :caps-lock :scroll-lock :num-lock :print-screen :pause
    :f1 :f2 :f3 :f4 :f5 :f6 :f7 :f8 :f9 :f10 :f11 :f12
    :left-shift :right-shift
    :left-control :right-control
    :left-alt :right-alt
    :left-super :right-super
    :left-meta :right-meta
    :left-hyper :right-hyper
    :kb-menu
    :keypad-0 :keypad-1 :keypad-2
    :keypad-3 :keypad-4 :keypad-5
    :keypad-6 :keypad-7 :keypad-8
    :keypad-9 :keypad-decimal :keypad-divide
    :keypad-multiply :keypad-subtract
    :keypad-add :keypad-enter :keypad-equal))

(deftype mouse-button () '(member :left :right :middle))


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
  (setf (gethash class-name *backends*) (list :priority (if testing most-positive-fixnum
                                                            priority)
                                              :class-name class-name)))

;;; A BACKEND SHOULD BE A CLASS THAT IMPLEMENT THESE FUNCTIONS
(defgeneric backend-init-window               (ctx width height title callback-handler-instance))
(defgeneric backend-close-window              (ctx))
(defgeneric backend-window-should-close-p     (ctx))
(defgeneric backend-begin-drawing             (ctx))
(defgeneric backend-end-drawing               (ctx))
(defgeneric backend-draw-rectangle            (ctx x y w h color))
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
(defgeneric backend-draw-canvas-on-canvas     (ctx canvas x y w h &optional tint))

(deftype redraw-frequency-type () `(member :target-fps :on-input))

(defclass window-state ()
  ((backend :accessor backend)
   (keyboard-state :accessor keyboard-state :initform (make-hash-table :test 'eq :size 256))
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
    :initform (make-array 256 :element-type 'symbol :fill-pointer 0 :initial-element nil)
    :documentation "A vector of all the keys which have been pressed this frame")
   (released-keys
    :accessor released-keys
    :initform (make-array 256 :element-type 'symbol :fill-pointer 0 :initial-element nil)
    :documentation "A vector of all the keys which have been released this frame")
   (pressed-mouse-buttons
    :accessor pressed-mouse-buttons
    :initform (make-array 3 :element-type 'symbol :fill-pointer 0 :initial-element nil))
   (released-mouse-buttons
    :accessor released-mouse-buttons
    :initform (make-array 3 :element-type 'symbol :fill-pointer 0 :initial-element nil))
   (target-fps :accessor target-fps :initform 60)
   (fps-history :accessor fps-history :initform (make-array 60 :adjustable t
                                                               :fill-pointer 0))
   (redraw-frequency :accessor redraw-frequency :initform :on-input :type redraw-frequency-type)
   (last-frame-timestamp :accessor last-frame-timestamp :initform (local-time:now))
   (current-frame-timestamp :accessor current-frame-timestamp :initform (local-time:now))
   (delta-time-seconds :accessor delta-time-seconds :initform 0)
   (input-happened-p :accessor input-happened-p :initform t)
   (draw-on-canvas? :accessor draw-on-canvas? :initform nil
                    :documentation "When non-nil, draw commands should apply to this
                                    canvas instead of the window")))



(defmethod callback-on-mouse-move ((handler window-state) x y)
  (unless (and (= (mouse-x handler) x)
               (= (mouse-y handler) y))
    (setf (input-happened-p handler) t)
    (setf (mouse-x handler) x)
    (setf (mouse-y handler) y)))

(defmethod callback-on-mouse-down ((handler window-state) mouse-button)
  (setf (input-happened-p handler) t)
  (vector-push mouse-button (pressed-mouse-buttons handler))
  (setf (gethash mouse-button (mouse-button-states handler)) t))

(defmethod callback-on-mouse-up ((handler window-state) mouse-button)
  (setf (input-happened-p handler) t)
  (vector-push mouse-button (released-mouse-buttons handler))
  (setf (gethash mouse-button (mouse-button-states handler)) nil))

(defmethod callback-on-key-down ((handler window-state) key)
  (declare (type key key))
  (setf (input-happened-p handler) t)
  (vector-push key (pressed-keys handler))
  (setf (gethash key (keyboard-state handler)) t))

(defmethod callback-on-key-up  ((handler window-state) key)
  (declare (type key key))
  (setf (input-happened-p handler) t)
  (vector-push key (released-keys handler))
  (setf (gethash key (keyboard-state handler)) nil))

(defmethod callback-all-keys-up ((handler window-state))
  (setf (input-happened-p handler) t)
  (loop :for key :across (pressed-keys handler)
        :do (callback-on-key-up handler key)
        :finally (setf (fill-pointer (pressed-keys handler)) 0)))

(defmethod callback-on-window-resize ((handler window-state) width height)
  (unless (and (= (window-width handler) width)
               (= (window-height handler) height))
    (setf (input-happened-p handler) t)
    (setf (window-width handler) width)
    (setf (window-height handler) height)))

;;; ==== PUBLIC INTERFACE ====
(defun init-window (width height title)
  "Attempts to initialize a window on your platform"
  (let ((prioritized-backends (sort (hash-table-values *backends*)
                                    (lambda (lhs rhs)
                                      (>
                                       (getf lhs :priority)
                                       (getf rhs :priority)))))
        (window (make-instance 'window-state)))
    (dolist (backend-info prioritized-backends)
      (let* ((instance (make-instance (getf backend-info :class-name))))
        (when-let (backend (ignore-errors (backend-init-window instance width height title window)))
          (setf (backend window) backend)
          (return-from init-window window)))))
  (error "No appropriate backend found :("))

(declaim (ftype (function (window-state) t) close-window))
(defun close-window (window-state)
  (backend-close-window (backend window-state)))

(declaim (ftype (function (window-state) fixnum) get-mouse-x))
(defun get-mouse-x (window-state)
  (mouse-x window-state))

(declaim (ftype (function (window-state) fixnum) get-mouse-y))
(defun get-mouse-y (window-state)
  (mouse-y window-state))

(declaim (ftype (function (window-state) fixnum) get-window-width))
(defun get-window-width (window-state)
  (window-width window-state))

(declaim (ftype (function (window-state) fixnum) get-window-height))
(defun get-window-height (window-state)
  (window-height window-state))

(declaim (ftype (function (window-state mouse-button) boolean) is-mouse-button-down))
(defun is-mouse-button-down (window-state button)
  (gethash button (mouse-button-states window-state)))

(declaim (ftype (function (window-state mouse-button) boolean) is-mouse-button-up))
(defun is-mouse-button-up (window-state button)
  (not (gethash button (mouse-button-states window-state))))

(declaim (ftype (function (window-state mouse-button) boolean) is-mouse-button-pressed))
(defun is-mouse-button-pressed (window-state button)
  (make-boolean (find button (pressed-mouse-buttons window-state))))

(declaim (ftype (function (window-state mouse-button) boolean) is-mouse-button-released))
(defun is-mouse-button-released (window-state button)
  (make-boolean (find button (released-mouse-buttons window-state))))

(declaim (ftype (function (window-state key) boolean) is-key-down))
(defun is-key-down (window-state key)
  (make-boolean (gethash key (keyboard-state window-state) nil)))

(declaim (ftype (function (window-state key) boolean) is-key-up))
(defun is-key-up (window-state key)
  (make-boolean (not (gethash key (keyboard-state window-state) nil))))

(declaim (ftype (function (window-state key) boolean) is-key-pressed))
(defun is-key-pressed (window-state key)
  (make-boolean (find key (pressed-keys window-state))))

(declaim (ftype (function (window-state key) boolean) is-key-released))
(defun is-key-released (window-state key)
  (make-boolean (find key (released-keys window-state))))

(declaim (ftype (function (window-state) t) begin-drawing))
(defun begin-drawing (window-state)
  (with-slots (backend last-frame-timestamp current-frame-timestamp delta-time-seconds)
      window-state
    
    (setf last-frame-timestamp current-frame-timestamp)
    (setf current-frame-timestamp (local-time:now))
    (setf delta-time-seconds
          (local-time:timestamp-difference current-frame-timestamp
                                           last-frame-timestamp))
    (backend-begin-drawing backend)))

(defun get-fps (window-state)
  (ignore-errors (/ 1 (delta-time-seconds window-state))))

(defun get-delta-time (window-state)
  "Returns delta time in seconds"
  (delta-time-seconds window-state))

(defun get-fps-string (window-state)
  (format nil "~a FPS" (floor (get-fps window-state))))

(defun get-seconds-passed-in-frame (window-state)
  (with-slots (current-frame-timestamp) window-state
    (local-time:timestamp-difference (local-time:now) current-frame-timestamp)))

(defun get-target-seconds-per-frame (window-state)
  (with-slots (target-fps) window-state
    (/ 1 target-fps)))

(defun get-remaining-seconds-in-frame (window-state)
  (- (get-target-seconds-per-frame window-state)
     (get-seconds-passed-in-frame window-state)))

(declaim (ftype (function (window-state) t) end-drawing))
(defun end-drawing (window-state)
  (backend-end-drawing (backend window-state))
  (setf (fill-pointer (pressed-keys window-state)) 0)
  (setf (fill-pointer (released-keys window-state)) 0)
  (setf (fill-pointer (pressed-mouse-buttons window-state)) 0)
  (setf (fill-pointer (released-mouse-buttons window-state)) 0)
  (setf (old-window-width window-state) (window-width window-state))
  (setf (old-window-height window-state) (window-height window-state))

  (ecase (redraw-frequency window-state)
    (:target-fps
     (let ((remaining (get-remaining-seconds-in-frame window-state)))
       (when (> remaining 0.01)
         (trivial-garbage:gc)
         (setf remaining (- (get-remaining-seconds-in-frame window-state) 0.001)))
       (when (plusp remaining)
         (sleep remaining))))
    (:on-input
     (loop :until (input-happened-p window-state)
           :do (sleep 0.001)
               (backend-check-for-input (backend window-state))
           :finally (setf (input-happened-p window-state) nil)))))

(declaim (ftype (function (window-state) boolean) window-should-close-p]))
(defun window-should-close-p (window-state)
  (backend-window-should-close-p (backend window-state)))

(declaim (ftype (function (window-state) boolean) window-should-keep-running-p))
(defun window-should-keep-running-p (window-state)
  (not (backend-window-should-close-p (backend window-state))))

(declaim (ftype (function (window-state number number number number color) t) draw-rectangle))
(defun draw-rectangle (window-state x y w h color)
  (with-slots (backend draw-on-canvas?) window-state
    (if draw-on-canvas?
        (backend-draw-rectangle-on-canvas window-state draw-on-canvas? x y w h color)
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

(declaim (ftype (function (window-state number number t &optional color) t) draw-canvas))
(defun draw-canvas (window-state x y canvas &optional tint)
  (with-slots (backend draw-on-canvas?) window-state
    (if draw-on-canvas?
        (backend-draw-canvas-on-canvas backend draw-on-canvas? x y canvas tint)
        (backend-draw-canvas backend x y canvas tint))))


;;; CANVAS
(declaim (ftype (function (window-state number number) t)))
(defun create-canvas (window-state width height)
  (backend-create-canvas (backend window-state) (floor width) (floor height)))

(declaim (ftype (function (window-state t) t) destroy-canvas))
(defun destroy-canvas (window-state canvas)
  (backend-destroy-canvas (backend window-state) canvas))

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
  (backend-set-preferred-text-height (backend window-state) (round text-height)))


;;; REDRAW AND FPS
(declaim (ftype (function (window-state redraw-frequency-type &optional number) t) set-redraw-frequency))
(defun set-redraw-frequency (window-state redraw-frequency-type &optional frames-per-second)
  (ecase redraw-frequency-type
    (:target-fps
     (assert frames-per-second () "expected target frames-per-second")
     (setf (target-fps window-state) frames-per-second)
     (setf (redraw-frequency window-state) :target-fps))
    (:on-input
     (assert (not frames-per-second))
     (setf (redraw-frequency window-state) :on-input))))

(defmacro with-window (name (width height title) &body body)
  `(let ((,name (init-window ,width ,height ,title)))
     (unwind-protect
          (progn ,@body)
       (close-window ,name))))

(defmacro with-drawing (state &body body)
  `(progn
     (begin-drawing ,state)
     (unwind-protect (progn ,@body)
       (end-drawing ,state))))

(defmacro while-running (state &body body)
  `(loop :while (window-should-keep-running-p ,state)
         :do ,@body))





(defun key->char (key)
  "Returns the corresponding character if possible or nil otherwise"
  (case key
    (:a #\a)
    (:b #\b)
    (:c #\c)
    (:d #\d)
    (:e #\e)
    (:f #\f)
    (:g #\g)
    (:h #\h)
    (:i #\i)
    (:j #\j)
    (:k #\k)
    (:l #\l)
    (:m #\m)
    (:n #\n)
    (:o #\o)
    (:p #\p)
    (:q #\q)
    (:r #\r)
    (:s #\s)
    (:t #\t)
    (:u #\u)
    (:v #\v)
    (:w #\w)
    (:x #\x)
    (:y #\y)
    (:z #\z)

    (:zero  #\0)
    (:one   #\1)
    (:two   #\2)
    (:three #\3)
    (:four  #\4)
    (:five  #\5)
    (:six   #\6)
    (:seven #\7)
    (:eight #\8)
    (:nine  #\9)

    (:quote        #\')
    (:comma        #\,)
    (:minus        #\-)
    (:period       #\.)
    (:slash        #\/)
    (:semicolon    #\;)
    (:equal        #\=)
    (:left-bracket #\[)
    (:right-bracket #\])
    (:backslash    #\\)
    (:backtick     #\`)
    (:space        #\Space)

    (:escape #\Esc)))


(defun char->key (char)
  "Convert an ascii terminal input character into a flat list of keys/modifiers.
   The list represents all possible keys/modifiers that could have produced CHAR."
  (case char
    ;; letters
    (#\a '(:a))
    (#\b '(:b))
    (#\c '(:c))
    (#\d '(:d))
    (#\e '(:e))
    (#\f '(:f))
    (#\g '(:g))
    (#\h '(:h))
    (#\i '(:i))
    (#\j '(:j))
    (#\k '(:k))
    (#\l '(:l))
    (#\m '(:m))
    (#\n '(:n))
    (#\o '(:o))
    (#\p '(:p))
    (#\q '(:q))
    (#\r '(:r))
    (#\s '(:s))
    (#\t '(:t))
    (#\u '(:u))
    (#\v '(:v))
    (#\w '(:w))
    (#\x '(:x))
    (#\y '(:y))
    (#\z '(:z))

    ;; shifted letters
    (#\A '(:a :left-shift))
    (#\B '(:b :left-shift))
    (#\C '(:c :left-shift))
    (#\D '(:d :left-shift))
    (#\E '(:e :left-shift))
    (#\F '(:f :left-shift))
    (#\G '(:g :left-shift))
    (#\H '(:h :left-shift))
    (#\I '(:i :left-shift))
    (#\J '(:j :left-shift))
    (#\K '(:k :left-shift))
    (#\L '(:l :left-shift))
    (#\M '(:m :left-shift))
    (#\N '(:n :left-shift))
    (#\O '(:o :left-shift))
    (#\P '(:p :left-shift))
    (#\Q '(:q :left-shift))
    (#\R '(:r :left-shift))
    (#\S '(:s :left-shift))
    (#\T '(:t :left-shift))
    (#\U '(:u :left-shift))
    (#\V '(:v :left-shift))
    (#\W '(:w :left-shift))
    (#\X '(:x :left-shift))
    (#\Y '(:y :left-shift))
    (#\Z '(:z :left-shift))

    ;; digits
    (#\0 '(:zero))
    (#\1 '(:one))
    (#\2 '(:two))
    (#\3 '(:three))
    (#\4 '(:four))
    (#\5 '(:five))
    (#\6 '(:six))
    (#\7 '(:seven))
    (#\8 '(:eight))
    (#\9 '(:nine))

    ;; shifted digits
    (#\) '(:zero :left-shift))
    (#\! '(:one  :left-shift))
    (#\@ '(:two  :left-shift))
    (#\# '(:three :left-shift))
    (#\$ '(:four :left-shift))
    (#\% '(:five :left-shift))
    (#\^ '(:six  :left-shift))
    (#\& '(:seven :left-shift))
    (#\* '(:eight :left-shift))
    (#\( '(:nine :left-shift))

    ;; punctuation
    (#\' '(:quote))
    (#\, '(:comma))
    (#\- '(:minus))
    (#\. '(:period))
    (#\/ '(:slash))
    (#\; '(:semicolon))
    (#\= '(:equal))
    (#\[ '(:left-bracket))
    (#\] '(:right-bracket))
    (#\\ '(:backslash))
    (#\` '(:backtick))
    (#\Space '(:space))

    ;; shifted punctuation
    (#\" '(:quote :left-shift))
    (#\< '(:comma :left-shift))
    (#\_ '(:minus :left-shift))
    (#\> '(:period :left-shift))
    (#\? '(:slash :left-shift))
    (#\: '(:semicolon :left-shift))
    (#\+ '(:equal :left-shift))
    (#\{ '(:left-bracket :left-shift))
    (#\} '(:right-bracket :left-shift))
    (#\| '(:backslash :left-shift))
    (#\~ '(:backtick :left-shift))

    ;; special keys
    (#\Tab        '(:tab))
    (#\Newline   '(:enter))
    (#\Return    '(:enter))
    (#\Backspace '(:backspace))
    (#\Esc       '(:escape
                   :left-bracket :left-control
                   :three :left-control))

    ;; control aliases
    (#\Nul '(:two :left-control))       ; Ctrl+2

    (#\Fs  '(:backslash :left-control
             :four :left-control))      ; Ctrl+\ / Ctrl+4

    (#\Gs  '(:right-bracket :left-control
             :five :left-control))      ; Ctrl+] / Ctrl+5

    (#\Rs  '(:six :left-control))       ; Ctrl+6

    (#\Us  '(:minus :left-control
             :seven :left-control))     ; Ctrl+_ / Ctrl+7

    (otherwise
     nil)))
