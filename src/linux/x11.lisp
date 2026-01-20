(defpackage #:clgfw/x11
  (:use #:cl #:clgfw/common)
  (:export #:init-window
           #:close-window
           #:window-should-keeping-running
           #:begin-drawing
           #:end-drawing
           #:draw-rectangle
           #:get-mouse-x
           #:get-mouse-y
           #:is-mouse-button-down
           #:get-window-width
           #:get-window-height))
(in-package #:clgfw/x11)

(defclass ctx ()
  ((window-should-keep-running  :accessor window-should-keep-running :initform t)
   (wm-delete-atom :accessor wm-delete-atom)
   (mouse-left-button-down :accessor mouse-left-button-down :initform nil)
   (mouse-middle-button-down :accessor mouse-middle-button-down :initform nil)
   (mouse-right-button-down :accessor mouse-right-button-down :initform nil)
   (black :accessor black)
   (white :accessor white)
   (font :accessor font)
   (display :accessor display)
   (screen :accessor screen)
   (window :accessor window)
   (gcontext :accessor gcontext)
   (colormap :accessor colormap)
   (keyboard-state :accessor keyboard-state :initform (make-hash-table :test 'eq :size 256))
   ))

(defun init-window (width height title &aux ctx)
  "Initialize the x11 window and return the created ctx"
  (declare (ignorable width height title))
  (setf ctx (make-instance 'ctx))
  (with-slots (black white font display screen window gcontext colormap) ctx
    (setf display (xlib:open-default-display))
    (setf screen (first (xlib:display-roots display)))
    (setf black (xlib:screen-black-pixel screen))
    (setf white (xlib:screen-white-pixel screen))
    (setf window (xlib:create-window
                  :x 10
                  :y 10
                  :width width
                  :height height
                  :background black
                  :parent (xlib:screen-root screen)
                  :event-mask (xlib:make-event-mask
                               :leave-window
                               :exposure
                               :property-change
                               :structure-notify
                               :button-press
                               :button-release
                               :key-press
                               )))
    (xlib::set-wm-protocols
     window
     '("WM_DELETE_WINDOW"))
    (xlib:set-wm-properties
     window
     :name title
     :width width
     :height height
     )
    (setf gcontext (xlib:create-gcontext
                    :drawable window
                    :background black
                    :foreground white))
    (setf (wm-delete-atom ctx) (xlib:intern-atom display "WM_DELETE_WINDOW"))
    (xlib:map-window window)
    (setf colormap (xlib:screen-default-colormap screen))
    
    (xlib:display-finish-output display)
    (xlib:display-force-output display)
    ctx)
  )

(defun draw-rectangle (ctx x y width height color)
  (let* ((r (color-r color))
         (g (color-g color))
         (b (color-b color))
         (hash (format nil "~a-~a-~a" r g b))
         (colormap (colormap ctx))
         (pixel (ignore-errors (xlib:lookup-color colormap hash))))
    (unless pixel
      (setf pixel (xlib:alloc-color colormap
                                    (xlib:make-color 
                                     :red (/ r 256)  
                                     :green (/ g 256)
                                     :blue (/ b 256)))))
    (setf (xlib:gcontext-foreground (gcontext ctx)) pixel)
    (xlib:draw-rectangle (window ctx) (gcontext ctx) x y width height t))
  )

(defun get-mouse-x (ctx)
  (multiple-value-bind (x) (xlib:query-pointer (window ctx)) x))

(defun get-mouse-y (ctx)
  (multiple-value-bind (x y) (xlib:query-pointer (window ctx))
    (declare (ignore x))
    y))

(defun is-mouse-button-down (ctx button)
  (ecase button
    (:left (mouse-left-button-down ctx))
    (:right (mouse-right-button-down ctx))
    (:middle (mouse-middle-button-down ctx))))

(defun get-window-width (ctx)
  (xlib:drawable-width (window ctx)))

(defun get-window-height (ctx)
  (xlib:drawable-height (window ctx)))

(defun begin-drawing (ctx)
  (declare (ignore ctx)))

(defun handle-key-press (ctx code)
  (error "TODO")

  ;;; Reference Keysym definitions from clx
  #|
  (define-keysym :character-set-switch character-set-switch-keysym)
  (define-keysym :left-shift left-shift-keysym)
  (define-keysym :right-shift right-shift-keysym)
  (define-keysym :left-control left-control-keysym)
  (define-keysym :right-control right-control-keysym)
  (define-keysym :caps-lock caps-lock-keysym)
  (define-keysym :shift-lock shift-lock-keysym)
  (define-keysym :left-meta left-meta-keysym)
  (define-keysym :right-meta right-meta-keysym)
  (define-keysym :left-alt left-alt-keysym)
  (define-keysym :right-alt right-alt-keysym)
  (define-keysym :left-super left-super-keysym)
  (define-keysym :right-super right-super-keysym)
  (define-keysym :left-hyper left-hyper-keysym)
  (define-keysym :right-hyper right-hyper-keysym)

  (define-keysym #\space 032)
  (define-keysym #\! 033)
  (define-keysym #\" 034)
  (define-keysym #\# 035)
  (define-keysym #\$ 036)
  (define-keysym #\% 037)
  (define-keysym #\& 038)
  (define-keysym #\' 039)
  (define-keysym #\( 040)
  (define-keysym #\) 041)
  (define-keysym #\* 042)
  (define-keysym #\+ 043)
  (define-keysym #\, 044)
  (define-keysym #\- 045)
  (define-keysym #\. 046)
  (define-keysym #\/ 047)
  (define-keysym #\0 048)
  (define-keysym #\1 049)
  (define-keysym #\2 050)
  (define-keysym #\3 051)
  (define-keysym #\4 052)
  (define-keysym #\5 053)
  (define-keysym #\6 054)
  (define-keysym #\7 055)
  (define-keysym #\8 056)
  (define-keysym #\9 057)
  (define-keysym #\: 058)
  (define-keysym #\; 059)
  (define-keysym #\< 060)
  (define-keysym #\= 061)
  (define-keysym #\> 062)
  (define-keysym #\? 063)
  (define-keysym #\@ 064)
  (define-keysym #\A 065 :lowercase 097)
  (define-keysym #\B 066 :lowercase 098)
  (define-keysym #\C 067 :lowercase 099)
  (define-keysym #\D 068 :lowercase 100)
  (define-keysym #\E 069 :lowercase 101)
  (define-keysym #\F 070 :lowercase 102)
  (define-keysym #\G 071 :lowercase 103)
  (define-keysym #\H 072 :lowercase 104)
  (define-keysym #\I 073 :lowercase 105)
  (define-keysym #\J 074 :lowercase 106)
  (define-keysym #\K 075 :lowercase 107)
  (define-keysym #\L 076 :lowercase 108)
  (define-keysym #\M 077 :lowercase 109)
  (define-keysym #\N 078 :lowercase 110)
  (define-keysym #\O 079 :lowercase 111)
  (define-keysym #\P 080 :lowercase 112)
  (define-keysym #\Q 081 :lowercase 113)
  (define-keysym #\R 082 :lowercase 114)
  (define-keysym #\S 083 :lowercase 115)
  (define-keysym #\T 084 :lowercase 116)
  (define-keysym #\U 085 :lowercase 117)
  (define-keysym #\V 086 :lowercase 118)
  (define-keysym #\W 087 :lowercase 119)
  (define-keysym #\X 088 :lowercase 120)
  (define-keysym #\Y 089 :lowercase 121)
  (define-keysym #\Z 090 :lowercase 122)
  (define-keysym #\[ 091)
  (define-keysym #\\ 092)
  (define-keysym #\] 093)
  (define-keysym #\^ 094)
  (define-keysym #\_ 095)
  (define-keysym #\` 096)
  (define-keysym #\a 097)
  (define-keysym #\b 098)
  (define-keysym #\c 099)
  (define-keysym #\d 100)
  (define-keysym #\e 101)
  (define-keysym #\f 102)
  (define-keysym #\g 103)
  (define-keysym #\h 104)
  (define-keysym #\i 105)
  (define-keysym #\j 106)
  (define-keysym #\k 107)
  (define-keysym #\l 108)
  (define-keysym #\m 109)
  (define-keysym #\n 110)
  (define-keysym #\o 111)
  (define-keysym #\p 112)
  (define-keysym #\q 113)
  (define-keysym #\r 114)
  (define-keysym #\s 115)
  (define-keysym #\t 116)
  (define-keysym #\u 117)
  (define-keysym #\v 118)
  (define-keysym #\w 119)
  (define-keysym #\x 120)
  (define-keysym #\y 121)
  (define-keysym #\z 122)
  (define-keysym #\{ 123)
  (define-keysym #\| 124)
  (define-keysym #\} 125)
  (define-keysym #\~ 126)

  (progn   ;; Semi-standard characters
    (define-keysym #\rubout (keysym 255 255))	; :tty
    (define-keysym #\tab (keysym 255 009))	; :tty
    (define-keysym #\linefeed (keysym 255 010))	; :tty
    (define-keysym #\page (keysym 009 227))	; :special
    (define-keysym #\return (keysym 255 013))	; :tty
    (define-keysym #\backspace (keysym 255 008))	; :tty
    )
  |#
  )

(defun end-drawing (ctx &aux display)
  (setf display (display ctx))
  (xlib:display-force-output display)
  (when (xlib:event-listen display)
    (xlib:event-case (display)
      ;; (:resize-request (width height)
      ;;                  (format t "Window resized to ~a/~a~%" width height)
      ;;                  (setf (xlib:drawable-height (window ctx)) height)
      ;;                  (setf (xlib:drawable-width (window ctx)) width))
      (:key-press (code)
                  (format t "TODO handle keypress: ~S~%" (xlib:keysym->character
                                                          display
                                                          (xlib:keycode->keysym
                                                           display
                                                           code
                                                           (xlib:default-keysym-index display code 0))))
                  t)
      (:button-press (code)
                     (ecase code
                       (1 (setf (mouse-left-button-down ctx) t))
                       (2 (setf (mouse-middle-button-down ctx) t))
                       (3 (setf (mouse-right-button-down ctx) t)))
                     t
                     )
      (:button-release (code)
                       (ecase code
                         (1 (setf (mouse-left-button-down ctx) nil))
                         (2 (setf (mouse-middle-button-down ctx) nil))
                         (3 (setf (mouse-right-button-down ctx) nil)))
                       t
                       )
      (:client-message (type data)
                       ;; TYPE is an atom
                       ;; DATA is a vector of 32-bit values
                       (when (and (eq type :wm_protocols)
                                  (eq (aref data 0) (wm-delete-atom ctx)))
                         (setf (window-should-keep-running ctx) nil)
                         (return-from end-drawing))
                       t)
      (:destroy-notify ()
                       (setf (window-should-keep-running ctx) nil)
                       (return-from end-drawing))
      (t () t)))
  )

(defun close-window (ctx)
  (xlib:close-display (display ctx) :abort nil))
