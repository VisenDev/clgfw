;; (eval-when (:load-toplevel :compile-toplevel :execute)
;;   ;;; automatically load clx for convenience when testing in repl
;;   (unless (find-package 'xlib)
;;     (asdf:load-system "clx")))

(in-package #:clgfw)

(defclass state ()
  ((window-should-keep-running  :accessor window-should-keep-running :initform t)
   (wm-delete-atom :accessor wm-delete-atom)
   (black :accessor black)
   (white :accessor white)
   (font :accessor font)
   (display :accessor display)
   (screen :accessor screen)
   (window :accessor window)
   (gcontext :accessor gcontext)
   (colormap :accessor colormap)))

(defun init-window (width height title &aux state)
  (declare (ignorable width height title))
  (setf state (make-instance 'state))
  (with-slots (black white font display screen window gcontext colormap) state
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
                               :key-press)))
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
    (setf (wm-delete-atom state) (xlib:intern-atom display "WM_DELETE_WINDOW"))
    (xlib:map-window window)
    (setf colormap (xlib:screen-default-colormap screen))
    
    (xlib:display-finish-output display)
    (xlib:display-force-output display)
    state)
  )

(defun draw-rectangle (state &key x y width height r g b)
  (let* ((hash (format nil "~a~a~a" r g b))
         (colormap (colormap state))
         (pixel (ignore-errors (xlib:lookup-color colormap hash))))
    (unless pixel
      (setf pixel (xlib:alloc-color colormap
                                    (xlib:make-color 
                                     :red (/ r 256)  
                                     :green (/ g 256)
                                     :blue (/ b 256)))))
    (setf (xlib:gcontext-foreground (gcontext state)) pixel)
    (xlib:draw-rectangle (window state) (gcontext state) x y width height t)))

(defun get-mouse-x (state)
  (multiple-value-bind (x) (xlib:query-pointer (window state)) x))

(defun get-mouse-y (state)
  (multiple-value-bind (x y) (xlib:query-pointer (window state))
    (declare (ignore x))
    y))

(defun begin-drawing (state)
  (declare (ignore state)))

(defun end-drawing (state)
  (xlib:display-force-output (display state))
  (when (xlib:event-listen (display state))
    (xlib:event-case ((display state))
      (:client-message (type data)
                       ;; TYPE is an atom
                       ;; DATA is a vector of 32-bit values
                       (when (and (eq type :wm_protocols)
                                  (eq (aref data 0) (wm-delete-atom state)))
                         (setf (window-should-keep-running state) nil)
                         (return-from end-drawing)))
      (:destroy-notify ()
                       (setf (window-should-keep-running state) nil)
                       (return-from end-drawing))
      (t () t)))
  )

(defun close-window (win)
  (xlib:close-display (display win) :abort nil))
