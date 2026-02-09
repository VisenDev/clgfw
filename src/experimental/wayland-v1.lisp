;;;; ==== CLGFW WAYLAND BACKEND ====
;;;; Copyright 2025, by Robert Burnett
;;;; Part of the CLGFW project

(in-package #:clgfw)

(defclass ctx/wayland ()
  (;; Globals
   (wl-display :accessor wl-display)
   (wl-registry :accessor wl-registry)
   (wl-shm :accessor wl-shm)
   (wl-compositor :accessor wl-compositor)
   (xdg-wm-base :accessor xdg-wm-base)
   wl-seat
   
   ;; Objects
   (wl-surface :accessor wl-surface)
   (xdg-surface :accessor xdg-surface)
   (xdg-toplevel :accessor xdg-toplevel)
   (wl-keyboard :initform nil)
   (wl-pointer :initform nil)
   cursor-surface

   ;; State
   (offset :initform 0.0)
   (last-frame :initform 0)
   (window-width :type wl-int)
   (window-height :type wl-int)
   (xkb-context :initform (cffi:null-pointer))
   (xkb-keymap :initform (cffi:null-pointer))
   (xkb-state :initform (cffi:null-pointer))
   (ptr-x :initform nil)
   (ptr-y :initform nil)
   (btn-left? :initform nil)
   (btn-right? :initform nil)
   (btn-mid? :initform nil)
   (radius :initform 50)
   latest-pointer-serial
   (custom-cursor-on? :initform nil)
   messages
   ))

(defparameter +min-radius+ 30)
(defparameter +max-radius+ 150)

(defun handle-pointer (ctx &rest event)
  (with-slots (wl-surface wl-pointer ptr-x ptr-y radius
               btn-left? btn-right? btn-mid?
               messages latest-pointer-serial
               cursor-surface custom-cursor-on?)
      ctx
    (wayflan-client:event-case event
      ;; Save the pointer position whenever it enters and moves the surface.
      ;; Since we only have one surface open, we ignore this value -- but
      ;; multi-surface apps can save this value to associate the pointer
      ;; with the relevant surface.
      (:enter (serial surface x y)
              (declare (ignore surface))
              (setf latest-pointer-serial serial)
              (setf ptr-x x
                    ptr-y y))
      (:motion (time-ms x y)
               (declare (ignore time-ms))
               (setf ptr-x x
                     ptr-y y))
      (:leave (serial surface)
              ;; Hide the pointer when it leaves the surface.
              (declare (ignore serial surface))
              (setf ptr-x nil
                    ptr-y nil))
      (:button (serial time-ms button state)
               (declare (ignore serial time-ms))
               ;; Wayland sends the rising and falling edges of button presses.
               ;; For the app, we want to show the current state of the button,
               ;; so save each button's state as it changes.
               (block nil
                 (setf (slot-value
                        ctx
                        (alexandria:switch (button :test #'=)
                          (input-event-codes:+btn-left+ 'btn-left?)
                          (input-event-codes:+btn-right+ 'btn-right?)
                          (input-event-codes:+btn-middle+ 'btn-mid?)
                          (t (return))))
                       (eq state :pressed)))

               (when (and (= button input-event-codes:+btn-middle+)
                          (eq state :released))
                 (setf custom-cursor-on? (not custom-cursor-on?))
                 (if custom-cursor-on?
                     (wayflan-client:wl-pointer.set-cursor wl-pointer latest-pointer-serial
                                                           cursor-surface 13 13)
                     (wayflan-client:wl-pointer.set-cursor wl-pointer latest-pointer-serial
                                                           nil 0 0))))
      (:axis (time-ms axis delta)
             ;; The value of the axis's motion is in the same coordinate space as
             ;; :MOTION events.
             (declare (ignore time-ms))
             (when (eq axis :vertical-scroll)
               (setf radius (alexandria:clamp (- radius delta) +min-radius+ +max-radius+))))
      (:frame ()
              ;; Intentionally left blank.
              ;;
              ;; The wl-pointer FRAME event groups separate events together, by
              ;; signifying the end of a logical group -- for example, an :AXIS-SOURCE
              ;; showing where a scroll is coming from, two :AXIS events for
              ;; :HORIZONTAL-SCROLL and :VERTICAL-SCROLL signifying a vertical scroll,
              ;; or a :LEAVE followed by an :ENTER to signify the pointer moved from
              ;; one surface to the other.
              ;;
              ;; This application is simple enough that it doesn't need to understand
              ;; logical event groups. Applications with more complex event processing
              ;; ought to accumulate each event, and then process them as a group once
              ;; :FRAME is heard.
              ))

;;     Redraw the surface
     (let ((buffer (draw-frame ctx)))
       (wayflan-client:wl-surface.attach wl-surface buffer 0 0)
       (wayflan-client:wl-surface.damage
         wl-surface 0 0 wayflan:+most-positive-wl-int+ wayflan:+most-positive-wl-int+)
       (wayflan-client:wl-surface.commit wl-surface))
    ))

(defun handle-keyboard (app &rest event)
  (with-slots (xkb-context xkb-keymap xkb-state press-map messages wl-surface) app
    (wayflan-client:event-case event
      ;; wl-keyboard's :KEYMAP event provides a key map as a file and size,
      ;; alongside the format used.
      (:keymap (format fd size)
               (let ((shm (posix-shm:make-shm fd)))
                 (unwind-protect
                      (progn
                        ;; wl_keyboard::keyboard_format exists in case a new
                        ;; keyboard format comes along, but for now, :XKB-V1 should
                        ;; be the only format compositors for any PC may send.
                        (assert (eq format :xkb-v1))
                        (posix-shm:with-mmap (ptr shm size :flags '(:private))
                          (let* ((keymap (xkb:xkb-keymap-new-from-string
                                          xkb-context ptr :text-v1 ()))
                                 (state (xkb:xkb-state-new keymap)))
                            (xkb:xkb-keymap-unref xkb-keymap)
                            (xkb:xkb-state-unref xkb-state)
                            (setf xkb-keymap keymap
                                  xkb-state state))))
                   (posix-shm:close-shm shm))))

      ;; Update changed state of keyboard modifiers
      (:modifiers (serial depressed latched locked group)
                  (declare (ignore serial))
                  (xkb:xkb-state-update-mask
                   xkb-state
                   depressed latched locked
                   0 0 group))

      ;; Notify keyboard focus entered the surface, and an array of keycodes
      ;; already entered.
      (:enter (serial surface keys)
              (declare (ignore serial surface))
              (format t "Keyboard entered -- keys pressed are:~%")
              (dotimes (i (length keys))
                (let* ((keycode (+ 8 (aref keys i)))
                       (sym (xkb:xkb-state-key-get-one-sym xkb-state keycode))
                       (msg (format nil "DOWN ~12A (~D), utf8: ~A"
                                    (xkb:xkb-keysym-get-name sym)
                                    sym
                                    (xkb:xkb-state-key-get-utf8 xkb-state keycode))))
                  ;; (update-press-map press-map sym :pressed) TODO fix
                  (princ msg) (terpri))))

      ;; Notify a key was pressed or released
      (:key (serial time-ms key state)
            (declare (ignore serial time-ms))
            (let* ((keycode (+ 8 key))
                   (sym (xkb:xkb-state-key-get-one-sym xkb-state keycode))
                   (msg (format nil "~4A ~12A (~D), utf8: ~A"
                                (if (eq state :pressed) "DOWN" "UP")
                                (xkb:xkb-keysym-get-name sym)
                                sym
                                (xkb:xkb-state-key-get-utf8 xkb-state keycode))))
              ;; (update-press-map press-map sym state)
              (princ msg) (terpri)))

      ;; Notify leave of focus
      (:leave (serial surface)
              (declare (ignore serial surface))
              (format t "Keyboard leave~%"))

      ;; Provides information on the user's preferred key repetition settings.
      ;; Implementation is left to the client.
      (:repeat-info (rate delay)
                    (format t "Preferred repeat: ~D keys/sec after ~Dms~%"
                            rate delay)))

;;     Redraw the surface
     (let ((buffer (draw-frame app)))
       (wayflan-client:wl-surface.attach wl-surface buffer 0 0)
       (wayflan-client:wl-surface.damage
        wl-surface 0 0 wayflan:+most-positive-wl-int+ wayflan:+most-positive-wl-int+)
       (wayflan-client:wl-surface.commit wl-surface))
    ))

(defun handle-seat (app &rest event)
  (with-slots (wl-seat wl-keyboard wl-pointer) app
    (wayflan-client:event-case event
      (:name (name)
             (format t "Seat name: ~S~%" name))
      (:capabilities (capabilities)
                     (format t "Wl-seat capabilities: ~S~%" capabilities)
                     (if (member :pointer capabilities)
                         (unless wl-pointer
                           (setf wl-pointer (wayflan-client:wl-seat.get-pointer wl-seat))
                           (push (alexandria:curry 'handle-pointer app)
                                 (wayflan-client:wl-proxy-hooks wl-pointer)))
                         (when wl-pointer
                           (wayflan-client:destroy-proxy wl-pointer)
                           (setf wl-pointer nil)))
                     (if (member :keyboard capabilities)
                         (unless wl-keyboard
                           (setf wl-keyboard (wayflan-client:wl-seat.get-keyboard wl-seat))
                           (push (alexandria:curry 'handle-keyboard app)
                                 (wayflan-client:wl-proxy-hooks wl-keyboard)))
                         (when wl-keyboard
                           (wayflan-client:destroy-proxy wl-keyboard)
                           (setf wl-keyboard nil)))))))

(defun handle-registry (ctx registry &rest event)
  (with-slots (wl-shm wl-compositor wl-seat xdg-wm-base) ctx
    (wayflan-client:event-case event
      (:global (name interface version)
               (declare (ignore version))
               ;; (format t "name: ~a, interface: ~a, version: ~a, interface-name: ~a~%"
               ;;         name interface version (wayflan-client:find-interface-named interface))
               (let ((interface-name (wayflan-client:find-interface-named interface)))
                 (when interface-name
                   (case (class-name interface-name)
                     (wayflan-client:wl-shm
                      (setf wl-shm (wayflan-client:wl-registry.bind
                                    registry name 'wayflan-client:wl-shm 1)))
                     (wayflan-client:wl-compositor
                      (format t "Compositor found!~%")
                      (setf wl-compositor (wayflan-client:wl-registry.bind
                                           registry name 'wayflan-client:wl-compositor 4)))
                     (wayflan-client:wl-seat
                      (setf wl-seat (wayflan-client:wl-registry.bind
                                     registry name 'wayflan-client:wl-seat 4))
                      (push (alexandria:curry 'handle-seat ctx)
                            (wayflan-client:wl-proxy-hooks wl-seat)))
                     (wayflan-client.xdg-shell:xdg-wm-base
                      (setf xdg-wm-base (wayflan-client:wl-registry.bind
                                         registry name 'wayflan-client.xdg-shell:xdg-wm-base 1))
                      (push (wayflan-client:evelambda
                              (:ping (serial)
                                     (wayflan-client.xdg-shell:xdg-wm-base.pong xdg-wm-base serial)))
                            (wayflan-client:wl-proxy-hooks xdg-wm-base)))
                     )
                   ))
               ))))

(defun draw-frame (app)
  (with-slots (wl-shm (width window-width) (height window-height) press-map messages) app
    (let* ((stride (* width 4))
           (size (* stride height))
           buffer)
      (posix-shm:with-open-shm-and-mmap* (shm pool-data (:direction :io) (size))
        (wayflan-client:with-proxy (pool (wayflan-client:wl-shm.create-pool wl-shm (posix-shm:shm-fd shm) size))
          (setf buffer (wayflan-client:wl-shm-pool.create-buffer
                         pool 0 width height stride :xrgb8888)))
        (break)
        ;; Draw the surface
        ;; (cairo:with-surface-and-context (surf (cairo:create-image-surface-for-data
        ;;                                         pool-data :rgb24 width height stride))
        ;;   ;; Draw a white background
        ;;   (cairo:set-source-rgb 1 1 1)
        ;;   (cairo:paint)

        ;;   (draw-key "W" (- width 300) 100 (plusp (sbit press-map +w+)))
        ;;   (draw-key "A" (- width 400) 200 (plusp (sbit press-map +a+)))
        ;;   (draw-key "S" (- width 300) 200 (plusp (sbit press-map +s+)))
        ;;   (draw-key "D" (- width 200) 200 (plusp (sbit press-map +d+)))

        ;;   (cairo:move-to 0 0)
        ;;   (pango:print-with-attributes
        ;;     ((format nil "Keyboard Events:~%~A" (rb-to-string messages))
        ;;      :width width)
        ;;     `((:absolute-size ,(min 30 (floor height 15)))
        ;;       (:family "Monospace"))))
        )

      (push (wayflan-client:evelambda
              (:release ()
               (wayflan-client:destroy-proxy buffer)))
            (wayflan-client:wl-proxy-hooks buffer))
      buffer)))

(defun init-window/wayland (width height title)
  (declare (ignore width height))
  (let ((ctx (make-instance 'ctx/wayland)))
    (with-slots (wl-display wl-registry wl-shm wl-compositor
                 xdg-wm-base wl-surface xdg-surface xdg-toplevel
                 window-width window-height xkb-context)
        ctx

      (setf xkb-context (xkb:xkb-context-new ())) ;removing this causes a segfault
      
      (setf wl-display (wayflan-client:wl-display-connect))
      (setf wl-registry (wayflan-client:wl-display.get-registry wl-display))
      (push (alexandria:curry 'handle-registry ctx wl-registry)
            (wayflan-client:wl-proxy-hooks wl-registry))

      (wayflan-client:wl-display-roundtrip wl-display)

      ;; set up a surface
      (setf wl-surface (wayflan-client:wl-compositor.create-surface wl-compositor)
            xdg-surface (wayflan-client.xdg-shell:xdg-wm-base.get-xdg-surface xdg-wm-base wl-surface)
            xdg-toplevel (wayflan-client.xdg-shell:xdg-surface.get-toplevel xdg-surface))
      (push (wayflan-client:evelambda
              (:configure (serial)
                          (wayflan-client.xdg-shell:xdg-surface.ack-configure xdg-surface serial)
                          (let ((buffer (draw-frame ctx)))
                            (wayflan-client:wl-surface.attach wl-surface buffer 0 0)
                            (wayflan-client:wl-surface.commit wl-surface))))
            (wayflan-client:wl-proxy-hooks xdg-surface))
      (push (wayflan-client:evlambda
              (:configure (new-width new-height state)
                          (declare (ignore state))
                          (setf window-width (if (zerop new-width) 800 new-width)
                                window-height (if (zerop new-height) 600 new-height)))
              (:close ()
                      ;; Close the app
                      (return-from init-window/wayland)))
            (wayflan-client:wl-proxy-hooks xdg-toplevel))

      ;;SET TITLE
      (wayflan-client.xdg-shell:xdg-toplevel.set-title xdg-toplevel title)
      (wayflan-client:wl-surface.commit wl-surface)

        ;; Keep handling events until we get a close signal
      (loop (wayflan-client:wl-display-dispatch-event wl-display))

      )
    )
  )


;; (defun init-window (width height title)
;;   (let ((state (make-instance 'state)))
;;     (with-slots (wl-display wl-registry wl-shm wl-compositor
;;                  xdg-wm-base wl-surface xdg-surface xdg-toplevel)
;;         state
;;       (wayflan:with-open-display (display)
;;         ;; Register all globals
;;         (setf wl-display display
;;               wl-registry (wayflan:wl-display.get-registry display))
;;         (push (curry 'handle-registry ctx wl-registry)
;;               (wl-proxy-hooks wl-registry))
;;         (wl-display-roundtrip display)

;;         ;; Create the surface & give it the toplevel role
;;         (setf wl-surface (wl-compositor.create-surface wl-compositor)
;;               xdg-surface (xdg-wm-base.get-xdg-surface
;;                            xdg-wm-base wl-surface)
;;               xdg-toplevel (xdg-surface.get-toplevel xdg-surface))
;;         (push (evlambda
;;                 (:close ()
;;                         (return-from run)))
;;               (wl-proxy-hooks xdg-toplevel))
;;         (push (evelambda
;;                 (:configure (serial)
;;                             (xdg-surface.ack-configure xdg-surface serial)
;;                             (let ((buffer (draw-frame app)))
;;                               (wl-surface.attach wl-surface buffer 0 0)
;;                               (wl-surface.commit wl-surface))))
;;               (wl-proxy-hooks xdg-surface))
;;         (xdg-toplevel.set-title xdg-toplevel "Example client")
;;         (wl-surface.commit wl-surface)

;;         (let ((cb (wl-surface.frame wl-surface)))
;;           (push (a:curry 'handle-frame-callback app cb)
;;                 (wl-proxy-hooks cb)))

;;         (loop (wl-display-dispatch-event display)))))
;;   )

  
