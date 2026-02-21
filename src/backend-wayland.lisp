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

#+sbcl (setq sb-ext:*block-compile-default* t)

(defpackage #:clgfw/backend/wayland
  (:use #:cl
        #:wayflan
        #:wayflan-client
        #:wayflan-client.xdg-shell
        #:clgfw/pixbuf)
  (:export #:backend/wayland))
(in-package #:clgfw/backend/wayland)

;;; This program is based on several examples from the wayflan repo
;;;
;;; This example is a rewrite of the extended example code found in Drew
;;; Devault's The Wayland Protocol, §7.3 thru §8.2. The program creates
;;; a toplevel surface that shows a moving checkerboard grid.

(clgfw:register-backend 'backend/wayland clgfw:+priority-primary+)

(defclass render-buffer ()
  ((pool-data :accessor pool-data
              :initarg :pool-data
              :documentation "This is the actual memory buffer that can be aref'd to write pixels")
   (buffer :accessor buffer
           :initarg :buffer
           :documentation "The wayland buffer made from the pool data")))

(defclass backend/wayland ()
  (;; Globals
   (wl-display :accessor wl-display)
   wl-registry
   wl-shm
   wl-compositor
   xdg-wm-base
   wl-seat

   ;; Objects
   wl-surface
   xdg-surface
   xdg-toplevel
   (wl-pointer :initform nil)
   (wl-keyboard :initform nil)
   (pool :initform nil)
   
   shm
   (backing-pool-data :accessor backing-pool-data :initform nil)
   (backing-pool-data-size :accessor backing-pool-data-size :initform nil)
   
   (front-buffer :accessor front-buffer :type render-buffer)
   (back-buffer :accessor back-buffer :type render-buffer)
   
   ;; State
   (xkb-context :initform (xkb:xkb-context-new ()))
   (xkb-keymap :initform (cffi:null-pointer))
   (xkb-state :initform (cffi:null-pointer))
   (handler :accessor handler :documentation "callback handler instance")
   (need-next-frame-p :accessor need-next-frame :initform t)
   (window-resized-p :accessor window-resized-p :initform t)
   (width :initform 640 :accessor width :type fixnum)
   (height :initform 480 :accessor height :type fixnum)
   (configured :accessor configured :initform nil)
   (window-should-close-p :initform nil :accessor window-should-close-p)
   (preferred-text-height :accessor preferred-text-height :initform 25
                          :documentation "At what size should text be drawn")))

(defmethod clgfw:backend-check-for-input ((ctx backend/wayland))
  (wl-display-dispatch-event (wl-display ctx)))

(defmethod clgfw:backend-window-should-close-p ((ctx backend/wayland))
  (window-should-close-p ctx))

(defmethod clgfw:backend-set-preferred-text-height ((ctx backend/wayland) text-height)
  (setf (preferred-text-height ctx) text-height))

(defun handle-pointer (ctx &rest event)
  (with-slots (width height mouse-x mouse-y
               buttons hover-cell) ctx
    (event-case event
      ;; Update pointer position
      (:enter (serial surface x y)
              (declare (ignore serial surface x y))
              ;; (setf mouse-x (floor x) mouse-y (floor y))
              )
      (:leave (serial surface)
              (declare (ignore serial surface))
              ;; (setf mouse-x 0 mouse-y 0)
              )
      (:motion (time-ms x y)
               (declare (ignore time-ms))
               (clgfw::callback-on-mouse-move (handler ctx) (floor x) (floor y)))
      ;; Update active cell state based on mousedown location
      (:button (serial time-ms button state)
               (declare (ignore serial time-ms))

               (let ((button (cond
                               ((= button input-event-codes:+btn-left+) :left)
                               ((= button input-event-codes:+btn-right+) :right)
                               ((= button input-event-codes:+btn-middle+) :middle))))
                 (if (eq state :pressed)
                     (clgfw::callback-on-mouse-down (handler ctx) button)
                     (clgfw::callback-on-mouse-up (handler ctx) button)))))))

(defun handle-seat (ctx &rest event)
  (with-slots (wl-seat wl-pointer wl-keyboard) ctx
    (event-case event
      (:capabilities (capabilities)
       (if (member :pointer capabilities)
           (unless wl-pointer
             (setf wl-pointer (wl-seat.get-pointer wl-seat))
             (push (alexandria:curry 'handle-pointer ctx)
                   (wl-proxy-hooks wl-pointer)))
           (when wl-pointer
             (destroy-proxy wl-pointer)
             (setf wl-pointer nil)))
       (if (member :keyboard capabilities)
           (unless wl-keyboard
             (setf wl-keyboard (wl-seat.get-keyboard wl-seat))
             (push (alexandria:curry 'handle-keyboard ctx)
                   (wl-proxy-hooks wl-keyboard))
             )
           (when wl-keyboard
             (destroy-proxy wl-keyboard)
             (setf wl-keyboard nil)))))))

(defun calculate-buffer-memory-needed (ctx)
  (let* ((stride (* (width ctx) 4))
         (buffer-size (* (height ctx) stride))
         (total-size (* buffer-size 2)))
    total-size))

(defun ensure-buffer-memory-allocated (ctx)
  (unless (window-resized-p ctx) (return-from ensure-buffer-memory-allocated))

  ;; Reset window resized flag
  (setf (window-resized-p ctx) nil)
  (format t "Reallocating memory due to window resize~%")
  
  (with-slots (shm wl-shm pool backing-pool-data
               backing-pool-data-size width height
               front-buffer back-buffer) ctx

    ;; Free old memory if allocated
    (unless (null backing-pool-data)
      (assert backing-pool-data-size)
      (assert pool)
      (wl-shm-pool.destroy pool)
      (posix-shm:munmap (backing-pool-data ctx) (backing-pool-data-size ctx)))
    
    ;; Allocate new memory  
    (let* ((stride (* width 4))
           (buffer-size (* height stride))
           (total-size (calculate-buffer-memory-needed ctx))
           )
      (unless (> (or (backing-pool-data-size ctx) 0) total-size)
        (posix-shm:truncate-shm shm total-size))
      (setf pool (wl-shm.create-pool wl-shm (posix-shm:shm-fd shm) total-size))
      (setf (backing-pool-data ctx) (posix-shm:mmap-shm shm total-size))
      (setf (backing-pool-data-size ctx) total-size)
      (setf front-buffer
            (make-instance 'render-buffer
                           :pool-data (backing-pool-data ctx)
                           :buffer (wl-shm-pool.create-buffer
                                    pool 0 width height stride :xrgb8888)))
      (setf back-buffer
            (make-instance 'render-buffer
                           :pool-data (cffi:inc-pointer (backing-pool-data ctx) buffer-size)
                           :buffer (wl-shm-pool.create-buffer
                                    pool buffer-size width height stride :xrgb8888)))
      (push (evelambda (:release ())) (wl-proxy-hooks (buffer front-buffer)))
      (push (evelambda (:release ())) (wl-proxy-hooks (buffer back-buffer)))
      )))

(defmethod clgfw:backend-begin-drawing ((ctx backend/wayland))
  (ensure-buffer-memory-allocated ctx)
  (loop :until (need-next-frame ctx)
        :do (sleep 0.001)
            (wl-display-dispatch-event (wl-display ctx))))

(defmethod clgfw:backend-end-drawing ((ctx backend/wayland))
  (when (window-should-close-p ctx)
    (return-from clgfw:backend-end-drawing))

  (ensure-buffer-memory-allocated ctx)
  
  (unless (configured ctx)
    (loop
      (wl-display-dispatch-event (wl-display ctx))
      (when (configured ctx) (return))))


  (setf (need-next-frame ctx) nil)
  (with-slots (wl-surface) ctx
    (let ((wl-buffer (buffer (back-buffer ctx))))
      (wl-surface.attach wl-surface wl-buffer 0 0)
      (wl-surface.damage-buffer
       wl-surface 0 0 +most-positive-wl-int+ +most-positive-wl-int+)
      (wl-surface.commit wl-surface)))

  ;;swap front and back buffer
  (let* ((fb (front-buffer ctx))
         (bb (back-buffer ctx)))
    (setf (front-buffer ctx) bb)
    (setf (back-buffer ctx) fb)))

;; (defun color->xrgb (color)
;;   (declare (type clgfw:color color))
;;   (let* ((result #xff)
;;          (result (ash result 8))
;;          (result (+ result (clgfw:color-r color)))
;;          (result (ash result 8))
;;          (result (+ result (clgfw:color-g color)))
;;          (result (ash result 8))
;;          (result (+ result (clgfw:color-b color)))
;;          )
;;     result))

;; (defun xrgb->color (xrgb)
;;   (declare (type (unsigned-byte 32) xrgb))
;;   (let ((r (ldb (byte 8 16) xrgb))
;;         (g (ldb (byte 8 8) xrgb))
;;         (b (ldb (byte 8 0) xrgb)))
;;     (clgfw:make-color r g b)))

(defun coordinates->offset (x y width)
  (declare (optimize (speed 3))
           (type fixnum x y width))
  (the fixnum (+ x (the fixnum (* y width)))))

(declaim (ftype (function (t fixnum fixnum fixnum fixnum clgfw/color:color))))
(defun draw-pixel (pool-data x y ctx-width ctx-height &key color xrgb)
  (declare (optimize (speed 3) (safety 1) (debug 0) (space 0))
           (type fixnum x y ctx-width ctx-height))
  (unless (and (< x ctx-width)
               (< y ctx-height))
    (return-from draw-pixel))

  (let ((offset (coordinates->offset x y ctx-width)))
    (unless xrgb
      (when (clgfw/color:color-invisible-p color)
        (return-from draw-pixel))
      (let* ((base-xrgb (cffi:mem-aref pool-data :uint32 offset))
             (base-color (clgfw/color:xrgb->color base-xrgb))
             (final-color (if (clgfw/color:color-opaque-p color) color
                              (clgfw/color:color-blend base-color color))))
        (setf xrgb (clgfw/color:color->xrgb final-color))))
    (setf (cffi:mem-aref pool-data :uint32 offset) xrgb)))

(defmethod clgfw:backend-draw-rectangle ((ctx backend/wayland) x y w h color)
  (declare (optimize (speed 3)))
  (ensure-buffer-memory-allocated ctx)
  (loop
    :with width :of-type fixnum = (floor (width ctx))
    :with height :of-type fixnum = (floor (height ctx))
    :with pool-data = (pool-data (back-buffer ctx))
    :with start-x :of-type fixnum = (coerce (max (round x) 0) 'fixnum)
    :with end-x :of-type fixnum = (coerce (min
                                           (+ (round x) (round w))
                                           (1- (round width)))
                                          'fixnum)
    :with start-y :of-type fixnum = (coerce (max (round y) 0) 'fixnum)
    :with end-y :of-type fixnum = (coerce (min
                                           (+ (round y) (round w))
                                           (1- (round height)))
                                          'fixnum)
    :with opaque = (clgfw/color:color-opaque-p color)
    :with xrgb = (clgfw/color:color->xrgb color)
    :for dx :from start-x :below end-x
    :do (loop
          :for dy :of-type fixnum :from start-y :below end-y
          :do (if opaque
                  (draw-pixel pool-data dx dy width height :xrgb xrgb)
                  (draw-pixel pool-data dx dy width height :color color))
          )))

;; (defmethod clgfw:backend-draw-rectangle ((ctx backend/wayland) x y w h color)
;;   (declare (optimize (speed 3) (safety 1) (debug 0) (space 0)))
;;   (when (or (zerop (width ctx))
;;             (zerop (height ctx)))
;;     (return-from clgfw:backend-draw-rectangle))

;;   (ensure-buffer-memory-allocated ctx)
;;   (let* ((pool-data (pool-data (back-buffer ctx)))
;;          (stride (the fixnum (* (width ctx) 4))) ; bytes per row
;;          (row-pixels (the fixnum (/ stride 4)))
;;          (x-end (the fixnum (floor (min (+ x w) (width ctx)))))
;;          (y-end (the fixnum (floor (min (+ y h) (height ctx))))))

;;     (if (clgfw:color-opaque-p color)
;;         (loop :with xrgb = (color-to-xrbg color)
;;               :for dy :from (the fixnum (round y)) :below y-end
;;               :do (loop :with dy-offset = (the fixnum (* (max 0 dy) row-pixels)) 
;;                         :for dx :from (the fixnum (max 0 (round x))) :below x-end
;;                         :do
;;                            (setf (cffi:mem-aref pool-data :uint32
;;                                                 (+ dx dy-offset))
;;                                  xrgb)))
;;         (loop :for dy :from (the fixnum (round y)) :below y-end
;;               :do (loop :with dy-offset = (the fixnum (* (max 0 dy) row-pixels)) 
;;                         :for dx :from (the fixnum (max 0 (round x))) :below x-end
;;                         :for base-color = (cffi:mem-aref pool-data :uint32
;;                                                          (+ dx dy-offset))
;;                         :for output-color = (clgfw:color-blend base-color color)
;;                         :for xrgb = (color-to-xrbg output-color)
;;                         :do
;;                            (setf (cffi:mem-aref pool-data :uint32
;;                                                 (+ dx dy-offset))
;;                                  xrgb))))))

(defun handle-frame-callback (ctx callback &rest event)
  (event-ecase event
    (:done (time)
           (declare (ignore time))
           (with-slots (last-frame offset wl-surface) ctx
             ;; Destroy this callback
             (destroy-proxy callback)

             ;; Request another frame
             (setf callback (wl-surface.frame wl-surface))
             (push (alexandria:curry 'handle-frame-callback ctx callback)
                   (wl-proxy-hooks callback))

             (setf (need-next-frame ctx) t)
             ))))

#.(set-dispatch-macro-character
    #\# #\K (lambda (s c n &aux str keysym)
              (declare (ignore c n))
              (setq str (read s))
              (setq keysym (xkb:xkb-keysym-from-name str '(:no-flags)))
              keysym
              ))

(declaim (ftype (function (integer) clgfw:key) convert-key))
(defun convert-key (sym)
  (alexandria:eswitch (sym :test '=)
    ;; letters
    (#K"A" :a)   (#K"a" :a)
    (#K"B" :b)   (#K"b" :b)
    (#K"C" :c)   (#K"c" :c)
    (#K"D" :d)   (#K"d" :d)
    (#K"E" :e)   (#K"e" :e)
    (#K"F" :f)   (#K"f" :f)
    (#K"G" :g)   (#K"g" :g)
    (#K"H" :h)   (#K"h" :h)
    (#K"I" :i)   (#K"i" :i)
    (#K"J" :j)   (#K"j" :j)
    (#K"K" :k)   (#K"k" :k)
    (#K"L" :l)   (#K"l" :l)
    (#K"M" :m)   (#K"m" :m)
    (#K"N" :n)   (#K"n" :n)
    (#K"O" :o)   (#K"o" :o)
    (#K"P" :p)   (#K"p" :p)
    (#K"Q" :q)   (#K"q" :q)
    (#K"R" :r)   (#K"r" :r)
    (#K"S" :s)   (#K"s" :s)
    (#K"T" :t)   (#K"t" :t)
    (#K"U" :u)   (#K"u" :u)
    (#K"V" :v)   (#K"v" :v)
    (#K"W" :w)   (#K"w" :w)
    (#K"X" :x)   (#K"x" :x)
    (#K"Y" :y)   (#K"y" :y)
    (#K"Z" :z)   (#K"z" :z)

    ;; numbers
    (#K"0" :zero)
    (#K"1" :one)
    (#K"2" :two)
    (#K"3" :three)
    (#K"4" :four)
    (#K"5" :five)
    (#K"6" :six)
    (#K"7" :seven)
    (#K"8" :eight)
    (#K"9" :nine)

    ;; punctuation
    (#K"minus" :minus)
    (#K"equal" :equal)
    (#K"semicolon" :semicolon)
    (#K"apostrophe" :quote)
    (#K"comma" :comma)
    (#K"period" :period)
    (#K"slash" :slash)
    (#K"grave" :backtick)
    (#K"bracketleft" :left-bracket)
    (#K"bracketright" :right-bracket)
    (#K"backslash" :backslash)

    ;; whitespace / control
    (#K"space" :space)
    (#K"Return" :enter)
    (#K"Escape" :escape)
    (#K"Tab" :tab)
    (#K"BackSpace" :backspace)
    (#K"Insert" :insert)
    (#K"Delete" :delete)
    (#K"Home" :home)
    (#K"End" :end)
    (#K"Page_Up" :page-up)
    (#K"Page_Down" :page-down)

    ;; arrows
    (#K"Left" :left)
    (#K"Right" :right)
    (#K"Up" :up)
    (#K"Down" :down)

    ;; modifiers
    (#K"Shift_L" :left-shift)
    (#K"Shift_R" :right-shift)
    (#K"Control_L" :left-control)
    (#K"Control_R" :right-control)
    (#K"Alt_L" :left-alt)
    (#K"Alt_R" :right-alt)
    (#K"Super_L" :left-super)
    (#K"Super_R" :right-super)
    (#K"Meta_L" :left-meta)
    (#K"Meta_R" :right-meta)
    (#K"Hyper_L" :left-hyper)
    (#K"Hyper_R" :right-hyper)

    ;; lock / system
    (#K"Caps_Lock" :caps-lock)
    (#K"Scroll_Lock" :scroll-lock)
    (#K"Num_Lock" :num-lock)
    (#K"Print" :print-screen)
    (#K"Pause" :pause)
    (#K"Menu" :kb-menu)

    ;; function keys
    (#K"F1" :f1)
    (#K"F2" :f2)
    (#K"F3" :f3)
    (#K"F4" :f4)
    (#K"F5" :f5)
    (#K"F6" :f6)
    (#K"F7" :f7)
    (#K"F8" :f8)
    (#K"F9" :f9)
    (#K"F10" :f10)
    (#K"F11" :f11)
    (#K"F12" :f12)

    ;; keypad
    (#K"KP_0" :keypad-0)
    (#K"KP_1" :keypad-1)
    (#K"KP_2" :keypad-2)
    (#K"KP_3" :keypad-3)
    (#K"KP_4" :keypad-4)
    (#K"KP_5" :keypad-5)
    (#K"KP_6" :keypad-6)
    (#K"KP_7" :keypad-7)
    (#K"KP_8" :keypad-8)
    (#K"KP_9" :keypad-9)
    (#K"KP_Decimal" :keypad-decimal)
    (#K"KP_Divide" :keypad-divide)
    (#K"KP_Multiply" :keypad-multiply)
    (#K"KP_Subtract" :keypad-subtract)
    (#K"KP_Add" :keypad-add)
    (#K"KP_Enter" :keypad-enter)
    (#K"KP_Equal" :keypad-equal)))

;;reset readtable
#.(setf *readtable* (copy-readtable nil))

(defun handle-keyboard (ctx &rest event)
  (with-slots (xkb-context xkb-keymap xkb-state) ctx
    (event-case event
      ;; Set or update the keyboard key-map
      (:keymap (format fd size)
               (let ((shm (posix-shm:make-shm fd)))
                 (unwind-protect
                      (progn
                        (assert (eq format :xkb-v1))
                        (posix-shm:with-mmap (ptr shm size :prot '(:read) :flags '(:private))
                          (let* ((keymap (xkb:xkb-keymap-new-from-string
                                          xkb-context ptr :text-v1 ()))
                                 (state (xkb:xkb-state-new keymap)))
                            (xkb:xkb-keymap-unref xkb-keymap)
                            (xkb:xkb-state-unref xkb-state)
                            (setf xkb-keymap keymap
                                  xkb-state state))))
                   (posix-shm:close-shm shm))))

      ;; Pass thru mod key updates to the xkb state machine
      (:modifiers (serial depressed latched locked group)
                  (declare (ignore serial))
                  (xkb:xkb-state-update-mask
                   xkb-state depressed latched locked 0 0 group))

      ;; Handle keys as they're pressed
      (:key (serial time-ms key state)
            (declare (ignore serial time-ms))
            (let* ((keycode (+ 8 key))
                   (sym (xkb:xkb-state-key-get-one-sym xkb-state keycode)))
              (when (plusp sym)
                (if (eq state :pressed)
                    (clgfw::callback-on-key-down (handler ctx) (convert-key sym))
                    (clgfw::callback-on-key-up (handler ctx) (convert-key sym)))))))))

(defun handle-registry (ctx registry &rest event)
  (with-slots (wl-shm wl-compositor xdg-wm-base wl-seat wl-registry) ctx
    (event-case event
      (:global (name interface version)
               (declare (ignore version))
               (case (alexandria:when-let ((it (find-interface-named interface)))
                       (class-name it))
                 (wl-shm
                  (format t "found shm~%")
                  (setf wl-shm (wl-registry.bind
                                registry name 'wl-shm 1)))
                 (wl-seat
                  (format t "Found seat~%")
                  (setf wl-seat (wl-registry.bind
                                 wl-registry name 'wl-seat 5))
                  (push (alexandria:curry 'handle-seat ctx)
                        (wl-proxy-hooks wl-seat)))
                 (wl-compositor
                  (format t "found compositor~%")
                  (setf wl-compositor (wl-registry.bind
                                       registry name 'wl-compositor 4)))
                 (xdg-wm-base
                  (format t "found xdg~%")
                  (setf xdg-wm-base (wl-registry.bind
                                     registry name 'xdg-wm-base 1))
                  (push (evelambda
                          (:ping (serial)
                                 (xdg-wm-base.pong xdg-wm-base serial)))
                        (wl-proxy-hooks xdg-wm-base))))))))

(defmethod clgfw:backend-close-window ((ctx backend/wayland))
  (ignore-errors
   (progn
     (with-slots (shm pool) ctx
       (posix-shm:close-shm shm)
       (wl-shm-pool.destroy pool)
       (posix-shm:munmap (backing-pool-data ctx) (backing-pool-data-size ctx)))
     (wl-display-disconnect (wl-display ctx)))))


;;; TODO: I need to configure what the initial cursor looks like

(defmethod clgfw:backend-init-window
    ((ctx backend/wayland) width height title callback-handler-instance)
  (declare (ignore width height))
  (setf (handler ctx) callback-handler-instance)
  (with-slots (wl-display wl-registry wl-shm wl-compositor
               xdg-wm-base wl-surface xdg-surface xdg-toplevel
               width height shm front-buffer back-buffer pool)
      ctx
    
    ;; Register all globals
    (setf wl-display (wl-display-connect)
          wl-registry (wl-display.get-registry wl-display))
    (push (alexandria:curry 'handle-registry ctx wl-registry)
          (wl-proxy-hooks wl-registry))
    (wl-display-roundtrip wl-display)

    ;; Allocate shm
    (setf shm (posix-shm:open-shm* :direction :io))
    (ensure-buffer-memory-allocated ctx)

    ;; Create the surface & give it the toplevel role
    (setf wl-surface (wl-compositor.create-surface wl-compositor)
          xdg-surface (xdg-wm-base.get-xdg-surface
                       xdg-wm-base wl-surface)
          xdg-toplevel (xdg-surface.get-toplevel xdg-surface))
    (push (evlambda
            (:close ()
                    (setf (window-should-close-p ctx) t)))
          (wl-proxy-hooks xdg-toplevel))
    (push (evelambda
            (:configure (serial)
                        (format t "configure received serial=~a~%" serial)
                        (xdg-surface.ack-configure xdg-surface serial)
                        (setf (configured ctx) t)
                        ))
          (wl-proxy-hooks xdg-surface))
    (push (evlambda
            (:configure (new-width new-height states)
                        (declare (ignore states))

                        (format t "Window resized to ~ax~a~%" new-width new-height)
                        (clgfw::callback-on-window-resize (handler ctx) new-width new-height)
                        ;; Adjust the height and draw a new frame
                        (if (or (zerop new-width) (zerop new-height))
                            (setf width 480 height 360)
                            (setf width new-width height new-height))
                        (setf (window-resized-p ctx) t)
                        )
            (:close ()
                    (setf (window-should-close-p ctx) t)))
          (wl-proxy-hooks xdg-toplevel))
    (xdg-toplevel.set-title xdg-toplevel title)
    (wl-surface.commit wl-surface)


    (let ((cb (wl-surface.frame wl-surface)))
      (push (alexandria:curry 'handle-frame-callback ctx cb)
            (wl-proxy-hooks cb)))
    (wl-display-roundtrip wl-display)
    (return-from clgfw:backend-init-window ctx)))

(defmethod clgfw:backend-draw-text ((ctx backend/wayland) x y color text)
  (clgfw/bdf:draw-string ctx clgfw/bdf:*fonts* x y (preferred-text-height ctx) color text))

(defmethod clgfw:backend-create-canvas ((ctx backend/wayland) w h)
  (create-pixbuf w h))

(defmethod clgfw:backend-destroy-canvas ((ctx backend/wayland) canvas)
  (declare (ignore ctx canvas)))

(defmethod clgfw:backend-draw-rectangle-on-canvas ((ctx backend/wayland) canvas x y w h color)
  (pixbuf-draw-rectangle canvas x y w h color))

(defmethod clgfw:backend-draw-canvas ((ctx backend/wayland) x y canvas &optional tint)
  (declare (optimize (speed 3)))
  (ensure-buffer-memory-allocated ctx)
  (let ((width (width ctx))
        (height (height ctx))
        (pool-data (pool-data (back-buffer ctx)))
        (my-x (round x))
        (my-y (round y)))
    (declare (type fixnum my-x my-y))
    (pixbuf-do-pixels (canvas dx dy pixel)
      (draw-pixel
       pool-data
       (the fixnum (+ my-x dx))
       (the fixnum (+ my-y dy))
       width height
       :color (if (and tint (not (clgfw/color:color-invisible-p pixel)))
                  (clgfw/color:color-blend pixel tint)
                  pixel)
       ))))
