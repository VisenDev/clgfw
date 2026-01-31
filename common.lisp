(in-package #:clgfw)

;;; ==== MACROS ====
(defmacro when-it (test &body body)
  "Anaphoric when, stores test result in `it`"
  `(let ((,(intern "IT") ,test))
     (when ,(intern "IT")
       ,@body)))

(defmacro unless-it (test &body body)
  "Anaphoric unless, stores test result in `it`"
  `(let ((it ,test))
     (unless it
       ,@body)))

(defmacro if-it (test then else)
  "Anaphoric if, stores test result in `it`"
  `(let ((it ,test))
     (if it
         ,then
         ,else)))

(defmacro appendf (target-list &rest other-lists)
  "Appends lists to the end of target-list"
  `(setf ,target-list (append ,target-list ,@other-lists)))

;;; ==== COLORS ====
(deftype color () '(integer 0 #xffffffff))

(defmacro define-color-byte-accessor (name offset)
  `(progn
     (defun ,name (color)
       (declare (type color color)
                (optimize (speed 3) (safety 0)))
       (ldb (byte 8 ,offset) color))
     (define-setf-expander ,name (color &environment env)
       (get-setf-expansion `(ldb (byte 8 ,,offset) ,color) env))))

(define-color-byte-accessor color-r 24)
(define-color-byte-accessor color-g 16)
(define-color-byte-accessor color-b 8)
(define-color-byte-accessor color-a 0)

(declaim (ftype (function (&optional fixnum fixnum fixnum fixnum) color) make-color))
(defun make-color (&optional (r 0) (g 0) (b 0) (a #xff))
  (declare (optimize (speed 3) (safety 0))
           (type fixnum r g b a))
  (let* ((result (the fixnum r))
         (result (the fixnum (ash result 8)))
         (result (the fixnum (logior result g)))
         (result (the fixnum (ash result 8)))
         (result (the fixnum (logior result b)))
         (result (the fixnum (ash result 8)))
         (result (the fixnum (logior result a))))
    result))

;; TODO no functions currently care about color-a, that should be changed
;; so that alpha values actually work
(defun color-invisible-p (color)
  (declare (type color color))
  (= 0 (color-a color)))

;;; RECTS
(defstruct rect
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (w 0 :type fixnum)
  (h 0 :type fixnum))

(defstruct (color-rect (:include rect))
  (color (make-color) :type color))

;;; PUBLIC INTERFACE

(deftype mouse-button () '(member :left :right :middle))

(defun init-window (width height title)
  "This is the entry point, it returns an appropriate ctx object 
   which can be used with the other generic functions to manipulate
   the window."
  #+abcl (funcall 'init-window/jvm width height title)
  #-abcl (progn
           #+linux(funcall 'init-window/linux width height title)
           #-linux (error "Only linux or abcl is supported right now"))
)



(defgeneric close-window (ctx))
(defgeneric window-should-keep-running (ctx)) ;; TODO maybe rename this to window-should-keep-running-p
(defgeneric begin-drawing (ctx))
(defgeneric end-drawing (ctx)) ;; TODO maybe schedule a gc to happen here so as to ensure a consistent framerate?

;;; Drawing
(defgeneric %get-draw-rectangle-function (ctx)
  (:documentation "Returns the actual function for the given backend that draws a 
                   rectangle. This is useful when optimizing a draw routine that
                   needs to draw many rectangles.
                   (You can avoid the generic function overhead)"))
(defgeneric draw-rectangle (ctx x y width height color)
  (:method (ctx (x fixnum) (y fixnum) (width fixnum) (height fixnum) color)
    (funcall (%get-draw-rectangle-function ctx) ctx x y width height color))
  (:method (ctx (x number) (y number) (width number) (height number) color)
    "Converts any non-fixnums to fixnums before calling the fixnum specific version"
    (funcall (%get-draw-rectangle-function ctx) ctx
                      (the fixnum (round x))
                      (the fixnum (round y))
                      (the fixnum (round width))
                      (the fixnum (round height))
                      color)))

(defgeneric get-mouse-x (ctx))
(defgeneric get-mouse-y (ctx))
(defgeneric get-window-width (ctx))
(defgeneric get-window-height (ctx))

(defgeneric is-mouse-button-down (ctx button))
(defgeneric is-key-down (ctx key))
(defgeneric is-key-pressed (ctx key)
  (:documentation "Like is-key-down, but only return true on the frame that the key is first pressed"))
(defgeneric is-key-released (ctx key)
  (:documentation "Like is-key-pressed, but only returns true when the key is released"))
(defgeneric set-target-fps (ctx)
  (:documentation "Adds a limit to how fast new frames should be drawn"))
(defgeneric get-delta-time (ctx)
  (:documentation "How many milliseconds of time have passed since the last frame"))
(defgeneric get-fps (ctx))
(defgeneric draw-text (ctx x y text-height color text)
  (:documentation "Draws some text on screen using the default font for the chosen backend"))

;;TODO add implementations for the following functions
(defgeneric measure-text-width (ctx text-height text)
  (:documentation "Returns the width needed if the input text were drawn on the screen"))
(defgeneric get-input-characters (ctx)
  (:documentation "Returns a vector containing character representations of every key that has 
                   been pressed this frame. This is intended for use in things like text input
                   widgets."))

(defgeneric create-image (ctx width height)
  (:documentation "An image can be used as the ctx for various draw functions, allowing
                   certain graphics to be saved and then drawn to the screen later"))
(defgeneric draw-image (ctx image x y)
  (:documentation "Draws the image at x y"))
(defgeneric destroy-image (image) ;; TODO investigate using trivial-garbage to call this fn
  (:documentation "Destroys the image, may be important if the image
                   is represented as a gpu texture in a given backend"))


;; Utilities defined using the above apis
(defun is-key-up (ctx key)
  "Inverse is-key-down"
  (not (is-key-down ctx key)))

(defmacro with-window (name (width height title) &body body)
  `(let ((,name (init-window ,width ,height ,title)))
     (unwind-protect
          (progn ,@body)
       (close-window ,name))))

(defmacro with-drawing (state &body body)
  `(progn
     (begin-drawing ,state)
     (unwind-protect (progn ,@body)
       (end-drawing ,state)))
  )

(defmacro while-running (state &body body)
  `(loop :while (window-should-keep-running ,state)
         :do ,@body))

(defmacro while-running/with-drawing (state &body body)
  `(while-running ,state
    (with-drawing ,state ,@body))
  )

(deftype key ()
  '(member
    :quote                              ; key: '
    :comma                              ; key: ,
    :minus                              ; key: -
    :period                             ; key: .
    :slash                              ; key: /
    :zero                               ; key: 0
    :one                                ; key: 1
    :two                                ; key: 2
    :three                              ; key: 3
    :four                               ; key: 4
    :five                               ; key: 5
    :six                                ; key: 6
    :seven                              ; key: 7
    :eight                              ; key: 8
    :nine                               ; key: 9
    :semicolon                          ; key: ;
    :equal                              ; key: =
    :a                                  ; key: a | A
    :b                                  ; key: b | B
    :c                                  ; key: c | C
    :d                                  ; key: d | D
    :e                                  ; key: e | E
    :f                                  ; key: f | F
    :g                                  ; key: g | G
    :h                                  ; key: h | H
    :i                                  ; key: i | I
    :j                                  ; key: j | J
    :k                                  ; key: k | K
    :l                                  ; key: l | L
    :m                                  ; key: m | M
    :n                                  ; key: n | N
    :o                                  ; key: o | O
    :p                                  ; key: p | P
    :q                                  ; key: q | Q
    :r                                  ; key: r | R
    :s                                  ; key: s | S
    :t                                  ; key: t | T
    :u                                  ; key: u | U
    :v                                  ; key: v | V
    :w                                  ; key: w | W
    :x                                  ; key: x | X
    :y                                  ; key: y | Y
    :z                                  ; key: z | Z
    :left-bracket                       ; key: [
    :backslash                          ; key: \
    :right-bracket                      ; key: ]
    :backtick                           ; key: `
    
    ;; function keys
    :space                              ; key: space
    :escape                             ; key: esc
    :enter                              ; key: enter
    :tab                                ; key: tab
    :backspace                          ; key: backspace
    :insert                             ; key: ins
    :delete                             ; key: del
    :right                              ; key: cursor right
    :left                               ; key: cursor left
    :down                               ; key: cursor down
    :up                                 ; key: cursor up
    :page-up                            ; key: page up
    :page-down                          ; key: page down
    :home                               ; key: home
    :end                                ; key: end
    :caps-lock                          ; key: caps lock
    :scroll-lock                        ; key: scroll down
    :num-lock                           ; key: num lock
    :print-screen                       ; key: print screen
    :pause                              ; key: pause
    :f1                                 ; key: f1
    :f2                                 ; key: f2
    :f3                                 ; key: f3
    :f4                                 ; key: f4
    :f5                                 ; key: f5
    :f6                                 ; key: f6
    :f7                                 ; key: f7
    :f8                                 ; key: f8
    :f9                                 ; key: f9
    :f10                                ; key: f10
    :f11                                ; key: f11
    :f12                                ; key: f12
    
    :left-shift                         ; key: shift left
    :right-shift                        ; key: shift right
    
    :left-control                       ; key: control left
    :right-control                      ; key: control right
    
    :left-alt                           ; key: alt left
    :right-alt                          ; key: alt right
    
    :left-super                         ; key: super left
    :right-super                        ; key: super right

    :left-meta                          ; key: meta left
    :right-meta                         ; key: meta right

    :left-hyper                         ; key: meta left
    :right-hyper                        ; key: meta right
    
    :kb-menu                            ; key: kb menu

    ;; keypad keys         
    :keypad-0                           ; key: keypad 0
    :keypad-1                           ; key: keypad 1
    :keypad-2                           ; key: keypad 2
    :keypad-3                           ; key: keypad 3
    :keypad-4                           ; key: keypad 4
    :keypad-5                           ; key: keypad 5
    :keypad-6                           ; key: keypad 6
    :keypad-7                           ; key: keypad 7
    :keypad-8                           ; key: keypad 8
    :keypad-9                           ; key: keypad 9
    :keypad-decimal                     ; key: keypad .
    :keypad-divide                      ; key: keypad /
    :keypad-multiply                    ; key: keypad *
    :keypad-subtract                    ; key: keypad -
    :keypad-add                         ; key: keypad +
    :keypad-enter                       ; key: keypad enter
    :keypad-equal                       ; key: keypad =
    ))


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
    (:space        #\Space))
)


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
    (#\Return :enter))
  )
