(defpackage #:clgfw/backend/x11
  (:use #:cl)
  (:export #:backend/x11))
(in-package #:clgfw/backend/x11)

(push 'backend/x11 clgfw:*backends*)

(defclass backend/x11 ()
  ((window-should-keep-running  :accessor window-should-keep-running :initform t)
   (wm-delete-atom :accessor wm-delete-atom)
   (handler :accessor handler
            :documentation "callback-handler-instance")
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
   (color-cache :accessor color-cache
                :initform (make-hash-table :size 128))
   ))

(defmethod clgfw:backend-window-should-close-p ((ctx backend/x11))
  (not (window-should-keep-running ctx)))

(defmethod clgfw:backend-init-window ((ctx backend/x11) width height title callback-handler-instance)
  "Initialize the x11 window and return the created ctx"
  (setf (handler ctx) callback-handler-instance)
  
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
    (xlib:display-force-output display)
    ctx)
  )

(declaim (ftype (function (backend/x11 clgfw:color) t) convert-to-x11-color))
(defun convert-to-x11-color (ctx color)
  "Computes a hash for the color to see if it is in the colormap already, otherwise, 
allocates the color"
  (declare (optimize (speed 3) (safety 0)))
  (clgfw:when-it (gethash color (color-cache ctx))
    (return-from convert-to-x11-color it))
  (format t "Allocating missing xlib color ~x :(~%" color)
  (setf (gethash color (color-cache ctx))
        (xlib:alloc-color (colormap ctx)
                          (xlib:make-color 
                           :red (the float (/ (clgfw:color-r color) 256f0))  
                           :green (the float (/ (clgfw:color-g color) 256f0))
                           :blue (the float (/ (clgfw:color-b color) 256f0)))))
  (convert-to-x11-color ctx color))

(defmethod clgfw:backend-draw-rectangle ((ctx backend/x11) x y width height color)
  (when (clgfw:color-invisible-p color)
    (return-from clgfw:backend-draw-rectangle))
  (setf (xlib:gcontext-foreground (gcontext ctx)) (convert-to-x11-color ctx color))
  (xlib:draw-rectangle (window ctx) (gcontext ctx) x y width height t))


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

(defmethod clgfw:backend-set-preferred-text-height ((ctx backend/x11) text-height)
  (clgfw:if-it (find-closest-xserver-font (display ctx) (round text-height))
               (setf (xlib:gcontext-font (gcontext ctx)) it)
               (setf (xlib:gcontext-font (gcontext ctx))
                     (first (xlib:list-font-names (display ctx) "*"))))

  (print (xlib:gcontext-font (gcontext ctx))))


(defmethod clgfw:backend-draw-text ((ctx backend/x11) x y color text)
  (with-slots (gcontext display) ctx
    (setf (xlib:gcontext-foreground gcontext) (convert-to-x11-color ctx color))
    (xlib:draw-glyphs (window ctx) (gcontext ctx) x (+ y 15) text)))


(defmethod clgfw:backend-begin-drawing ((ctx backend/x11))
  (declare (ignore ctx)))

(defun convert-keycode (ctx code)
  "Converts an X11 keycode to a clgfw key"
  (let* ((index (xlib:default-keysym-index (display ctx) code 0))
         (sym (xlib:keycode->keysym (display ctx) code index))
         (key
           (typecase sym
             (integer (clgfw:char->key (code-char sym)))
             (character (clgfw:char->key sym))
             (symbol (assert (typep sym 'clgfw:key)) sym)
             (t (error "unsupported type ~a" sym))
             )))
    key
    ))

#|
(defclass image/x11 ()
  ((pixmap :accessor pixmap :initarg :pixmap)
   (ctx :accessor ctx :initarg :ctx)))

(defmethod create-image ((ctx backend/x11) width height)
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

(defmethod draw-image ((ctx backend/x11) (image image/x11) x y)
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

(defmethod clgfw:backend-end-drawing ((ctx backend/x11) &aux display)
  (setf display (display ctx))
  
  (clgfw::callback-on-window-resize
   (handler ctx)
   (xlib:drawable-width (window ctx))
   (xlib:drawable-height (window ctx)))

  (multiple-value-bind (x y) (xlib:pointer-position (window ctx))
    (clgfw::callback-on-mouse-move (handler ctx) x y))
  
  (xlib:display-force-output display)
  (loop
    :while (xlib:event-listen display)
    :do
       (xlib:event-case (display)
         ;; (:resize-request (width height)
         ;;                  (format t "Window resized to ~a/~a~%" width height)
         ;;                  (setf (xlib:drawable-height (window ctx)) height)
         ;;                  (setf (xlib:drawable-width (window ctx)) width))
         (:key-press (code)
                     (clgfw:when-it (convert-keycode ctx code)
                       (clgfw::callback-on-key-down (handler ctx) it))
                     t)
         (:key-release (code)
                       (clgfw:when-it (convert-keycode ctx code)
                         (clgfw::callback-on-key-up (handler ctx) it))
                       t)
         (:button-press (code)
                        (clgfw::callback-on-mouse-down
                         (handler ctx)
                         (ecase code
                           (1 :left)
                           (2 :middle)
                           (3 :right)))
                        t)
         (:button-release (code)
                          (clgfw::callback-on-mouse-up
                           (handler ctx)
                           (ecase code
                             (1 :left)
                             (2 :middle)
                             (3 :right)))
                          t
                          )
         (:client-message (type data)
                          ;; TYPE is an atom
                          ;; DATA is a vector of 32-bit values
                          (when (and (eq type :wm_protocols)
                                     (eq (aref data 0) (wm-delete-atom ctx)))
                            (setf (window-should-keep-running ctx) nil)
                            (return-from clgfw::backend-end-drawing))
                          t)
         (:destroy-notify ()
                          (setf (window-should-keep-running ctx) nil)
                          (return-from clgfw::backend-end-drawing))
         (t () t)))
                       )

(defmethod clgfw:backend-close-window ((ctx backend/x11))
  (xlib:close-display (display ctx) :abort nil))


;; (defun image-text ()

;;   (with-window ctx (800 600 "image")
;;     (let ((image (create-image ctx 100 100)))
;;       (draw-rectangle image 10 10 20 20 (make-color 0 :b 200))
;;       (while-running/with-drawing ctx
;;         (draw-image ctx image 100 100)        
;;         ))))
