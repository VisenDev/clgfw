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

(declaim (ftype (function (color color) color) color-blend))
(defun color-blend (base applied-color)
  (declare (optimize (speed 3)
                     (safety 3)
                     (debug 3))
           (type color base applied-color))
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
    :keypad-add :keypad-enter :keypad-equal                       
    ))

(deftype mouse-button () '(member :left :right :middle))


;;; A CALLBACK HANDLER SHOULD IMPLEMENT THESE FUNCTIONS
(defgeneric callback-on-mouse-move    (handler x y))
(defgeneric callback-on-mouse-down    (handler mouse-button))
(defgeneric callback-on-mouse-up      (handler mouse-button))
(defgeneric callback-on-key-down      (handler key))
(defgeneric callback-on-key-up        (handler key))
(defgeneric callback-on-window-resize (handler width height))

;;; A BACKEND SHOULD IMPLEMENT THESE FUNCTIONS
(defgeneric backend-init-window               (ctx width height title callback-handler-instance))
(defgeneric backend-close-window              (ctx))
(defgeneric backend-window-should-close-p     (ctx))
(defgeneric backend-begin-drawing             (ctx))
(defgeneric backend-end-drawing               (ctx))
(defgeneric backend-draw-rectangle            (ctx x y w h color))
(defgeneric backend-set-preferred-text-height (ctx text-height))
(defgeneric backend-get-text-height           (ctx))
(defgeneric backend-measure-text-width        (ctx text))
(defgeneric backend-draw-text                 (ctx x y color string))
(defgeneric backend-draw-canvas               (ctx x y canvas &optional color))
(defgeneric backend-create-canvas             (ctx w h))
(defgeneric backend-destroy-canvas            (ctx canvas))
(defgeneric backend-canvas-draw-rectangle     (ctx canvas x y w h color))
(defgeneric backend-canvas-draw-text          (ctx canvas x y color))
(defgeneric backend-canvas-draw-canvas        (ctx canvas x y w h &optional tint))

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
   (mouse-left-button-down :accessor mouse-left-button-down :initform nil)
   (mouse-middle-button-down :accessor mouse-middle-button-down :initform nil)
   (mouse-right-button-down :accessor mouse-right-button-down :initform nil)
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
    :initform (make-array 3 :element-type 'symbol :fill-pointer 0 :initial-element nil))))

(defmethod callback-on-mouse-move ((handler window-state) x y)
  (setf (mouse-x handler) x)
  (setf (mouse-y handler) y))

(defmethod callback-on-mouse-down ((handler window-state) mouse-button)
  (vector-push mouse-button (pressed-mouse-buttons handler))
  (ecase (print mouse-button)
    (:left (setf (mouse-left-button-down handler) t))
    (:right (setf (mouse-right-button-down handler) t))
    (:middle (setf (mouse-middle-button-down handler) t))))

(defmethod callback-on-mouse-up ((handler window-state) mouse-button)
  (vector-push mouse-button (released-mouse-buttons handler))
  (ecase mouse-button
    (:left (setf (mouse-left-button-down handler) nil))
    (:right (setf (mouse-right-button-down handler) nil))
    (:middle (setf (mouse-middle-button-down handler) nil))))

(defmethod callback-on-key-down ((handler window-state) key)
  (declare (type key key))
  (vector-push key (pressed-keys handler))
  (setf (gethash key (keyboard-state handler)) t))

(defmethod callback-on-key-up  ((handler window-state) key)
  (declare (type key key))
  (vector-push key (released-keys handler))
  (setf (gethash key (keyboard-state handler)) nil))

(defmethod callback-on-window-resize ((handler window-state) width height)
  (setf (window-width handler) width)
  (setf (window-height handler) height))

;;; ==== PUBLIC INTERFACE ====

(defvar *backends* nil)

(defun init-window (width height title)
  "Attempts to initialize a window on your platform"
  (setf *backends* (delete-duplicates *backends*))
  (let ((window (make-instance 'window-state)))
    (dolist (backend *backends*)
      (let* ((instance (make-instance backend)))
        (when-it (backend-init-window instance width height title window)
          (setf (backend window) it)
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
  (ecase button
    (:left (mouse-left-button-down window-state))
    (:right (mouse-right-button-down window-state))
    (:middle (mouse-middle-button-down window-state))))

(declaim (ftype (function (window-state mouse-button) boolean) is-mouse-button-up))
(defun is-mouse-button-up (window-state button)
  (ecase button
    (:left (mouse-left-button-down window-state))
    (:right (mouse-right-button-down window-state))
    (:middle (mouse-middle-button-down window-state))))

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
  (backend-begin-drawing (backend window-state)))

(declaim (ftype (function (window-state) t) end-drawing))
(defun end-drawing (window-state)
  (setf (fill-pointer (pressed-keys window-state)) 0)
  (setf (fill-pointer (released-keys window-state)) 0)
  (setf (fill-pointer (pressed-mouse-buttons window-state)) 0)
  (setf (fill-pointer (released-mouse-buttons window-state)) 0)
  (setf (old-window-width window-state) (window-width window-state))
  (setf (old-window-height window-state) (window-height window-state))
  (backend-end-drawing (backend window-state)))

(declaim (ftype (function (window-state) boolean) window-should-close-p]))
(defun window-should-close-p (window-state)
  (backend-window-should-close-p (backend window-state)))

(declaim (ftype (function (window-state) boolean) window-should-keep-running-p))
(defun window-should-keep-running-p (window-state)
  (not (backend-window-should-close-p (backend window-state))))

(declaim (ftype (function (window-state number number number number color) t) draw-rectangle))
(defun draw-rectangle (window-state x y w h color)
  (backend-draw-rectangle (backend window-state) (floor x) (floor y) (floor w) (floor h) color))

(declaim (ftype (function (window-state number number color string) t) draw-text))
(defun draw-text (window-state x y color text)
  (backend-draw-text (backend window-state) (floor x) (floor y) color text))

(declaim (ftype (function (window-state number number t &optional color) t) draw-canvas))
(defun draw-canvas (window-state x y canvas &optional tint)
  (backend-draw-canvas (backend window-state) (floor x) (floor y) canvas tint))

(declaim (ftype (function (window-state number number) t)))
(defun create-canvas (window-state width height)
  (backend-create-canvas (backend window-state) (floor width) (floor height)))

(declaim (ftype (function (window-state t) t) destroy-canvas))
(defun destroy-canvas (window-state canvas)
  (backend-destroy-canvas (backend window-state) canvas))

(declaim (ftype (function (window-state t number number number number color) t) canvas-draw-rectangle))
(defun canvas-draw-rectangle (window-state canvas x y w h color)
  (backend-canvas-draw-rectangle (backend window-state) canvas (floor x) (floor y) (floor w) (floor h) color))

(declaim (ftype (function (window-state t number number color) t) canvas-draw-text))
(defun canvas-draw-text (window-state canvas x y color)
  (backend-canvas-draw-text (backend window-state) canvas (floor x) (floor y) color))

(declaim (ftype (function (window-state t number number t &optional color) t) canvas-draw-canvas))
(defun canvas-draw-canvas (window-state target-canvas x y source-canvas &optional tint)
  (backend-canvas-draw-canvas (backend window-state) target-canvas (floor x) (floor y) source-canvas tint))

(declaim (ftype (function (window-state number) t) set-preferred-text-height))
(defun set-preferred-text-height (window-state text-height)
  "Requests that the backend draws text at the given text-height. Might not always work because
   certain backends (ie clx) cannot draw arbitrary text sizes. Always use the text measuring
   functions to check the real size that text will be rendered at."
  (backend-set-preferred-text-height (backend window-state) (round text-height)))

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
  "Converts a character to a key, or returns nil"
  (case char
    ((#\a #\A) :a)
    ((#\b #\B) :b)
    ((#\c #\C) :c)
    ((#\d #\D) :d)
    ((#\e #\E) :e)
    ((#\f #\F) :f)
    ((#\g #\G) :g)
    ((#\h #\H) :h)
    ((#\i #\I) :i)
    ((#\j #\J) :j)
    ((#\k #\K) :k)
    ((#\l #\L) :l)
    ((#\m #\M) :m)
    ((#\n #\N) :n)
    ((#\o #\O) :o)
    ((#\p #\P) :p)
    ((#\q #\Q) :q)
    ((#\r #\R) :r)
    ((#\s #\S) :s)
    ((#\t #\T) :t)
    ((#\u #\U) :u)
    ((#\v #\V) :v)
    ((#\w #\W) :w)
    ((#\x #\X) :x)
    ((#\y #\Y) :y)
    ((#\z #\Z) :z)

    ((#\0 #\)) :zero)
    ((#\1 #\!) :one)
    ((#\2 #\@) :two)
    ((#\3 #\#) :three)
    ((#\4 #\$) :four)
    ((#\5 #\%) :five)
    ((#\6 #\^) :six)
    ((#\7 #\&) :seven)
    ((#\8 #\*) :eight)
    ((#\9 #\() :nine)

    (#\' :quote)
    (#\, :comma)
    (#\- :minus)
    (#\. :period)
    (#\/ :slash)
    (#\; :semicolon)
    (#\= :equal)
    (#\[ :left-bracket)
    (#\] :right-bracket)
    (#\\ :backslash)
    (#\` :backtick)
    (#\Space :space)
    (#\Return :enter)
    (#\Esc :escape)))
