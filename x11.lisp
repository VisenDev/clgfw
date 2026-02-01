(in-package #:clgfw)

(defclass ctx/x11 (fps-manager)
  ((window-should-keep-running  :accessor window-should-keep-running :initform t)
   (wm-delete-atom :accessor wm-delete-atom)
   (mouse-left-button-down :accessor mouse-left-button-down :initform nil)
   (mouse-middle-button-down :accessor mouse-middle-button-down :initform nil)
   (mouse-right-button-down :accessor mouse-right-button-down :initform nil)
   (black :accessor black)
   (white :accessor white)
   (font :accessor font :initform nil)
   (display :accessor display)
   (screen :accessor screen)
   (window :accessor window)
   (gcontext :accessor gcontext)
   (colormap :accessor colormap)
   (front-buffer :accessor front-buffer)
   (back-buffer :accessor back-buffer)
   (keyboard-state :accessor keyboard-state :initform (make-hash-table :test 'eq :size 256))
   (pressed-keys :accessor pressed-keys
                 :initform (make-array 256 :element-type 'symbol :fill-pointer 0 :initial-element nil)
                 :documentation "A vector of all the keys which have been pressed this frame")
   (released-keys :accessor released-keys
                 :initform (make-array 256 :element-type 'symbol :fill-pointer 0 :initial-element nil)
                 :documentation "A vector of all the keys which have been released this frame")
   (color-cache :accessor color-cache
                :initform (make-hash-table :size 128))
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

;; (define-condition broken-windowing-backend-warning (error) ())

(defun init-window/x11 (width height title &aux ctx)
  "Initialize the x11 window and return the created ctx"
  (declare (ignorable width height title))
   
  (setf ctx (make-instance 'ctx/x11))

  (setf (target-fps ctx) 30) ;; TODO remove this call
                             ;; The X server won't be able to keep up with higher fps
  
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
    ;; (setf (front-buffer ctx) (xlib:create-back-buffer ))
    
    
    ;; (xlib:display-finish-output display)
    (xlib:display-force-output display)
    ctx)
  )

(declaim (ftype (function (ctx/x11 color) xlib:color) convert-to-x11-color))
(defun convert-to-x11-color (ctx color)
  "Computes a hash for the color to see if it is in the colormap already, otherwise, 
allocates the color"
  (declare (optimize (speed 3) (safety 0)))
  (when-it (gethash color (color-cache ctx))
    (return-from convert-to-x11-color it))
  ;; (format t "Allocating missing xlib color ~x :(~%" color)
  (setf (gethash color (color-cache ctx))
        (xlib:alloc-color (colormap ctx)
                          (xlib:make-color 
                           :red (the float (/ (color-r color) 256f0))  
                           :green (the float (/ (color-g color) 256f0))
                           :blue (the float (/ (color-b color) 256f0)))))
  (convert-to-x11-color ctx color))

(declaim (ftype (function (ctx/x11 fixnum fixnum fixnum fixnum color) t) %draw-rectangle/x11))
(defun %draw-rectangle/x11 (ctx x y width height color)
  (when (color-invisible-p color)
    (return-from %draw-rectangle/x11))
  (setf (xlib:gcontext-foreground (gcontext ctx)) (convert-to-x11-color ctx color))
  (xlib:draw-rectangle (window ctx) (gcontext ctx) x y width height t))


(defmethod %get-draw-rectangle-function ((ctx ctx/x11))
  #'%draw-rectangle/x11)

(defun find-closest-xserver-font (display text-height)
  (declare (type integer text-height))
  (labels ((make-pattern (height)
             (format nil "*~a0*" height))
           (find-font (height)
             (first (xlib:list-fonts display (make-pattern height)))))
    (loop :for i :from 0 :below 100
          :for up :from text-height
          :for down :downfrom text-height
          :for up-font = (find-font up)
          :for down-font = (find-font (max 5 down))
          :do (when up-font (return (print up-font)))
              (when (and down-font (> down 5)) (return down-font)))))

#|
(defmethod draw-text ((ctx ctx/x11) x y text-height color text)
  (declare (ignorable text-height))
  (with-slots (gcontext display) ctx
    (setf (xlib:gcontext-foreground gcontext) (get-xlib-color ctx color))

    ;; I'm having trouble figuring out how to get the right sized xlib font
    
    ;; (setf (xlib:gcontext-font (gcontext ctx)) (find-closest-xserver-font display 100))
    (xlib:draw-glyphs (window ctx) gcontext(round x) (- (round y) 15) text)))
|#

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

(defun need-new-frame-buffers-p (ctx)
  (or (not (slot-boundp ctx 'front-buffer))
      (not (slot-boundp ctx 'back-buffer))
      ())
  )

(defmethod begin-drawing ((ctx ctx/x11))
  ;; (when (or (not (slot-boundp ))))
  (declare (ignore ctx))
  )

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

#|
(defclass image/x11 ()
  ((pixmap :accessor pixmap :initarg :pixmap)
   (ctx :accessor ctx :initarg :ctx)))

(defmethod create-image ((ctx ctx/x11) width height)
  (make-instance
   'image/x11
   :ctx ctx
   :pixmap
   (xlib:create-pixmap :width width
                       :height height
                       :depth (xlib:drawable-depth (window ctx))
                       :drawable (window ctx))))

(defmethod destroy-image ((image image/x11))
  (xlib:free-pixmap (pixmap image))
  (slot-makunbound image 'pixmap))

(defmethod draw-image ((ctx ctx/x11) (image image/x11) x y)
  (with-accessors ((pixmap pixmap)) image
    (xlib:copy-area pixmap (gcontext ctx) 0 0
                    (xlib:drawable-width pixmap) (xlib:drawable-height pixmap)
                    (window ctx) x y)))

(defmethod get-window-width ((image image/x11))
  (xlib:drawable-width (pixmap image)))

(defmethod get-window-height ((image image/x11))
  (xlib:drawable-height (pixmap image)))

(defmethod draw-rectangle ((image image/x11) x y width height color)
  (setf (xlib:gcontext-foreground (gcontext (ctx image)))
        ;;TODO replace this call to get-xlib-color
        (convert-to-x11-color (ctx image) color))
  (xlib:draw-rectangle (pixmap image)
                       (gcontext (ctx image))
                       (round x)
                       (round y)
                       (round width)
                       (round height)
                       t))
|#

(defmethod end-drawing ((ctx ctx/x11) &aux display)
  (setf display (display ctx))
  
  (setf (fill-pointer (pressed-keys ctx)) 0)  ;;reset pressed keys
  (setf (fill-pointer (released-keys ctx)) 0) ;;reset released keys

  ;;; TODO Implement double buffering
  ;; (xlib:display-finish-output display)

  (xlib:display-force-output display)
  (when (xlib:event-listen display)
    (xlib:event-case (display)
      ;; (:resize-request (width height)
      ;;                  (format t "Window resized to ~a/~a~%" width height)
      ;;                  (setf (xlib:drawable-height (window ctx)) height)
      ;;                  (setf (xlib:drawable-width (window ctx)) width))
      (:key-press (code)
                  (let ((key (convert-keycode ctx code)))
                    (when key
                      (vector-push (the symbol key) (pressed-keys ctx))
                      (setf (gethash key (keyboard-state ctx)) t)))
                  t)
      (:key-release (code)
                    (let ((key (convert-keycode ctx code)))
                      (when key
                        (vector-push (the symbol key) (released-keys ctx))
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



;; (defun image-text ()

;;   (with-window ctx (800 600 "image")
;;     (let ((image (create-image ctx 100 100)))
;;       (draw-rectangle image 10 10 20 20 (make-color 0 :b 200))
;;       (while-running/with-drawing ctx
;;         (draw-image ctx image 100 100)        
;;         ))))
