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
   (colormap :accessor colormap)
   visual
   glx-context
   (keyboard-state :accessor keyboard-state :initform (make-hash-table :test 'eq :size 256))))

(defmethod is-key-down ((ctx ctx/x11) key)
  (assert (typep key 'key) (key) "~a is not a valid clgfw key" key)
  (gethash key (keyboard-state ctx) nil))

(defun init-window/x11 (width height title &aux ctx root)
  "Initialize the x11 window and return the created ctx"
  (declare (ignorable width height title))
  (setf ctx (make-instance 'ctx/x11))
  (with-slots (black white font display screen window gcontext colormap visual glx-context) ctx
    
    (setf display (xlib:open-default-display))
    (setf screen (first (xlib:display-roots display)))
    (setf root (xlib:screen-root screen))
    
    ;;  (xlib/glx::client-info display) ;;tell server about us
    (setf visual (xlib/glx:choose-visual screen '(:glx-rgba
                                                       (:glx-red-size 1)
                                                       (:glx-green-size 1)
                                                       (:glx-blue-size 1)
                                                       :glx-double-buffer)))
    (print visual)
    (setf colormap (xlib:create-colormap 847 (xlib:screen-root screen)))
    (setf black (xlib:screen-black-pixel screen))
    (setf white (xlib:screen-white-pixel screen))
    (setf window (xlib:create-window
                  :x 10
                  :y 10
                  :width width
                  :height height
                  :background black
                  :parent root
                  :colormap colormap
;;                  :visual (xlib/glx:visual-id visual)
;;                  :visual #x27
                  :visual 847
                  :depth 24
                  :event-mask (xlib:make-event-mask
                               :leave-window
                               :exposure
                               :property-change
                               :structure-notify
                               :button-press
                               :button-release
                               :key-press
                               :key-release
                               )))
    (setf glx-context (xlib/glx:create-context screen #x27))
    (xlib:set-wm-properties window
                            :name title
                            :width width
                            :height height
                            :initial-state :normal
                            )
    (setf gcontext (xlib:create-gcontext
                    :drawable window
                    :background black
                    :foreground white))


    (xlib:map-window window)
    (xlib/glx:make-current window glx-context)
    (xlib:map-window window)

    (xlib::set-wm-protocols window '("WM_DELETE_WINDOW"))
    (setf (wm-delete-atom ctx) (xlib:intern-atom display "WM_DELETE_WINDOW"))
    (xlib:display-finish-output display)
    (xlib:display-force-output display)
    ctx)
  )

(defmethod draw-rectangle ((ctx ctx/x11) (x number) (y number)
                           (width number) (height number) (color color))

  (xlib/gl:color-3s (color-r color) (color-g color) (color-b color))
  (xlib/gl:rect-i x y (+ x width) (+ y width))
  (xlib/gl:end)
  ;; (let* ((r (color-r color))
  ;;        (g (color-g color))
  ;;        (b (color-b color))
  ;;        (hash (format nil "~a-~a-~a" r g b))
  ;;        (colormap (colormap ctx))
  ;;        (pixel (ignore-errors (xlib:lookup-color colormap hash))))
  ;;   (unless pixel
  ;;     (setf pixel (xlib:alloc-color colormap
  ;;                                   (xlib:make-color 
  ;;                                    :red (/ r 256)  
  ;;                                    :green (/ g 256)
  ;;                                    :blue (/ b 256)))))
  ;;   (setf (xlib:gcontext-foreground (gcontext ctx)) pixel)
  ;;   (xlib:draw-rectangle (window ctx) (gcontext ctx) x y width height t))
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

(defun convert-keycode (ctx code)
  "Converts an X11 keycode to a clgfw key"
  (let* ((index (xlib:default-keysym-index (display ctx) code 0))
         (sym (xlib:keycode->keysym (display ctx) code index)))
    (typecase sym
      (integer (char->key (code-char sym)))
      (character (char->key sym))
      (symbol (assert (typep sym 'key)) sym)
      (t (error "unsupported type ~a" sym))
      )))

(defmethod end-drawing ((ctx ctx/x11) &aux display)
  (setf display (display ctx))
  (xlib:display-force-output display)
  (xlib/glx:render)
  (xlib/glx:swap-buffers)
  
  ;; (loop :for key :being :each hash-key :of (keyboard-state ctx)
  ;;       :do (setf (gethash key (keyboard-state ctx)) nil))
  
  (when (xlib:event-listen display)
    (xlib:event-case (display)
      ;; (:resize-request (width height)
      ;;                  (format t "Window resized to ~a/~a~%" width height)
      ;;                  (setf (xlib:drawable-height (window ctx)) height)
      ;;                  (setf (xlib:drawable-width (window ctx)) width))
      (:key-press (code)
                  (format t "~a pressed~%" (convert-keycode ctx code))
                  (setf (gethash (convert-keycode ctx code) (keyboard-state ctx)) t)
                  t)
      (:key-release (code)
                    (format t "~a released~%" (convert-keycode ctx code))
                    (setf (gethash (convert-keycode ctx code) (keyboard-state ctx)) nil)
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
  (with-slots (glx-context) ctx
    (xlib/glx:destroy-context glx-context))
  (xlib:close-display (display ctx) :abort nil))
