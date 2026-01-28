;; (eval-when (:load-toplevel :compile-toplevel :execute)
;;   ;;; automatically load clx for convenience when testing in repl
;;   (unless (find-package 'xlib)
;;     (asdf:load-system "clx")))

(in-package #:clgfw)

(defclass ctx/x11 (fps-manager)
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
   (pressed-keys :accessor pressed-keys
                 :initform (make-array 256 :element-type 'symbol :fill-pointer 0 :initial-element nil)
                 :documentation "A vector of all the keys which have been pressed this frame")
   (released-keys :accessor released-keys
                 :initform (make-array 256 :element-type 'symbol :fill-pointer 0 :initial-element nil)
                 :documentation "A vector of all the keys which have been released this frame")
   ))

(defmethod is-key-down ((ctx ctx/x11) key)
  (assert (typep key 'key) (key) "~a is not a valid clgfw key" key)
  (gethash key (keyboard-state ctx) nil))

(defmethod is-key-pressed ((ctx ctx/x11) key)
  (assert (typep key 'key) (key) "~a is not a valid clgfw key" key)
  (find key (pressed-keys ctx)))

(defmethod is-key-released ((ctx ctx/x11) key)
  (assert (typep key 'key) (key) "~a is not a valid clgfw key" key)
  (find key (released-keys ctx)))

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
                               :key-release
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

(defclass color/x11 (color)
  ((xlib-color :accessor xlib-color)))

(defun hash-color (color)
  "computes a hash from a color"
  (let* ((hash (color-r color))
         (hash (ash hash 8))
         (hash (logior hash (color-b color)))
         (hash (ash hash 8))
         (hash (logior hash (color-g color))))
    hash))

(defun ensure-color-is-in-colormap (ctx color)
  "Computes a hash for the color to see if it is in the colormap already, otherwise, 
allocates the color"
  (let* ((r (color-r color))
         (g (color-g color))
         (b (color-b color))
         (colormap (colormap ctx))
         (color (ignore-errors (xlib:lookup-color colormap (hash-color color)))))
    (if color color
        (xlib:alloc-color colormap
                          (xlib:make-color 
                           :red (/ r 256)  
                           :green (/ g 256)
                           :blue (/ b 256))))))

(defun get-xlib-color (ctx color)
  (if (slot-exists-p color 'xlib-color)
      (slot-value color 'xlib-color)
      ;else
      (progn
        (change-class color 'color/x11)
        (setf (xlib-color color) (ensure-color-is-in-colormap ctx color))))
  )

(defmethod draw-rectangle ((ctx ctx/x11) x y
                           width height(color color))
  
  (setf (xlib:gcontext-foreground (gcontext ctx)) (get-xlib-color ctx color))
  (xlib:draw-rectangle (window ctx) (gcontext ctx) (round x) (round y) (round width) (round height) t)
  )

(defmethod draw-text ((ctx ctx/x11) x y text-height color text)
  ;;Warning, this function currently ignores text-height because there is not
  ;;an easy way to change text size in clx
  (declare (ignore text-height))
  (with-slots (gcontext) ctx
    (setf (xlib:gcontext-foreground gcontext) (get-xlib-color ctx color))
    ;; (setf (xlib:gcontext-font (gcontext ctx)) (get-xlib-font ctx text-height))
    (xlib:draw-glyphs (window ctx) gcontext(round x) (- (round y) 15) text)))

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
         (sym (xlib:keycode->keysym (display ctx) code index))
         (key
           (typecase sym
             (integer (char->key (code-char sym)))
             (character (char->key sym))
             (symbol (assert (typep sym 'key)) sym)
             (t (error "unsupported type ~a" sym))
             )))
    key
    ))

(defmethod end-drawing ((ctx ctx/x11) &aux display)
  (setf display (display ctx))
  
  (setf (fill-pointer (pressed-keys ctx)) 0) ;;reset pressed keys
  (setf (fill-pointer (released-keys ctx)) 0) ;;reset released keys

  (xlib:display-force-output display)
  (sleep 0.001)
  (when (xlib:event-listen display)
    (xlib:event-case (display)
      ;; (:resize-request (width height)
      ;;                  (format t "Window resized to ~a/~a~%" width height)
      ;;                  (setf (xlib:drawable-height (window ctx)) height)
      ;;                  (setf (xlib:drawable-width (window ctx)) width))
      (:key-press (code)
                  (let ((key (convert-keycode ctx code)))
                    (when key
                      (vector-push key (pressed-keys ctx))
                      (setf (gethash key (keyboard-state ctx)) t)))
                  t)
      (:key-release (code)
                    (let ((key (convert-keycode ctx code)))
                      (when key
                        (vector-push key (released-keys ctx))
                        (setf (gethash key (keyboard-state ctx)) nil)))
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
