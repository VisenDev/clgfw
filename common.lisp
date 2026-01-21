(in-package #:clgfw)

(defclass color ()
  ((r :initarg :r :accessor color-r :initform 0 :type (integer 0 255))
   (g :initarg :g :accessor color-g :initform 0 :type (integer 0 255))
   (b :initarg :b :accessor color-b :initform 0 :type (integer 0 255))))

(defun make-color (&key (r 0) (b 0) (g 0))
  (make-instance 'color :r r :g g :b b)
  )

(deftype mouse-button () '(member :left :right :middle))

;; Perhaps add init-window as a generic function?
(defgeneric close-window (ctx))
(defgeneric window-should-keep-running (ctx))
(defgeneric begin-drawing (ctx))
(defgeneric end-drawing (ctx))
(defgeneric draw-rectangle (ctx x y width height color))
(defgeneric get-mouse-x (ctx))
(defgeneric get-mouse-y (ctx))
(defgeneric is-mouse-button-down (ctx button))
(defgeneric is-key-down (ctx key))
(defgeneric get-window-width (ctx))
(defgeneric get-window-height (ctx))

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
    (:backslash    #\Backslash)
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

    (#\0 :zero)
    (#\1 :one)
    (#\2 :two)
    (#\3 :three)
    (#\4 :four)
    (#\5 :five)
    (#\6 :six)
    (#\7 :seven)
    (#\8 :eight)
    (#\9 :nine)

    (#\' :quote)
    (#\, :comma)
    (#\- :minus)
    (#\. :period)
    (#\/ :slash)
    (#\; :semicolon)
    (#\= :equal)
    (#\[ :left-bracket)
    (#\] :right-bracket)
    (#\Backslash :backslash)
    (#\` :backtick)
    (#\Space :space)
    (#\Return :enter))
  )
