;; (eval-when (:load-toplevel :compile-toplevel :execute)
;;   ;;; automatically load clx for convenience when testing in repl
;;   (unless (find-package 'xlib)
;;     (asdf:load-system "clx")))

(in-package #:clgfw)

(defclass ctx/x11 ()
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
   (colormap :accessor colormap)))

(defun init-window/x11 (width height title &aux ctx)
  "Initialize the x11 window and return the created ctx"
  (declare (ignorable width height title))
  (setf ctx (make-instance 'ctx/x11))
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

(defmethod draw-rectangle ((ctx ctx/x11) (x number) (y number)
                           (width number) (height number) (color color))
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

(defmethod get-mouse-x ((ctx ctx/x11))
  (multiple-value-bind (x) (xlib:query-pointer (window ctx)) x))

(defmethod get-mouse-y ((ctx ctx/x11))
  (multiple-value-bind (x y) (xlib:query-pointer (window ctx))
    (declare (ignore x))
    y))

(defmethod is-mouse-button-down ((ctx ctx/x11) (button symbol))
  (ecase button
    (:left (mouse-left-button-down ctx))
    (:right (mouse-right-button-down ctx))
    (:middle (mouse-middle-button-down ctx))))

(defmethod get-window-width ((ctx ctx/x11))
  (xlib:drawable-width (window ctx)))

(defmethod get-window-height ((ctx ctx/x11))
  (xlib:drawable-height (window ctx)))

(defmethod begin-drawing ((ctx ctx/x11))
  (declare (ignore ctx)))

(defmethod end-drawing ((ctx ctx/x11) &aux display)
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

(defmethod close-window ((ctx ctx/x11))
  (xlib:close-display (display ctx) :abort nil))
