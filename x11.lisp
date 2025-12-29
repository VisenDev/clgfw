(eval-when (:load-toplevel :compile-toplevel :execute)
  ;;; automatically load clx for convenience when testing in repl
  (unless (find-package 'xlib)
    (asdf:load-system "clx")))

(defpackage #:clgfw/x11
  (:use #:cl)
  (:export #:init-window
           #:close-window
           #:window-should-keeping-running
           #:begin-drawing
           #:end-drawing
           #:draw-rectangle
           #:get-mouse-x
           #:get-mouse-y))
(in-package #:clgfw/x11)

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



;;; HELPER CONVENIENCES
;;; These should probably be moved to a common file rather than in x11 specifically
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

(defmacro while-running-with-drawing (state &body body)
  `(loop :while (window-should-keep-running ,state)
         :do (with-drawing ,state ,@body)))


;;; EXAMPLE
(defun test-main ()
  "Example main function"
  (with-window state (100 100 "Hello")
    (while-running-with-drawing state
      (draw-rectangle state
                      :x (get-mouse-x state) :y (get-mouse-y state)
                      :width 10 :height 10
                      :r 200 :g 100 :b 100))))


;; (defun test-main ()
;;   (let* ((win (init-window 100 100 "foo"))
;;          (display (display win))
;;          (wm-delete (xlib:intern-atom display "WM_DELETE_WINDOW")))
;;     (unwind-protect                                              
;;          (loop :while t
;;                :do
;;                   (test-render win)
;;                   (format t "frame~%")
;;                   (when (xlib:event-listen display)
;;                     (xlib:event-case (display)
;;                       (:client-message (type data)
;;                                        ;; TYPE is an atom
;;                                        ;; DATA is a vector of 32-bit values
;;                                        (when (and (eq type :wm_protocols)
;;                                                   (eq (elt data 0) wm-delete))
;;                                          (format t "WM_DELETE_WINDOW received~%")
;;                                          (return-from test-main)))
;;                       (:destroy-notify ()
;;                                        (format t "Attempted to quit!~%")
;;                                        (return-from test-main))
;;                       (t () t)))
;;                   (sleep 0.01)
;;                )                                           
;;       (close-window win))                                      
    
;;     ;; (loop
;;     ;;   (test-render win))
;;     ;; (close-window win)
;;     ))
    
;;     ;; (unwind-protect
;;     ;;      (loop :while 
;;     ;;            (xlib:event-case ((display win))
;;     ;;              (:exposure
;;     ;;               (count)
;;     ;;               (test-render win))
;;     ;;              (:destroy-notify ()
;;     ;;                               (format t "window closed~%"))
;;     ;;              ))
;;     ;;   (close-window win))
;;     ))




;;(defun init-window (width height title)
;;  (let* ((s (make-instance 'state))
;;         )
;;    (setf (display s) (xlib:open-default-display))
;;	  (setf (screen s) (xlib:display-default-screen (display s)))
;;    (setf (black s) (xlib:screen-black-pixel (screen s)))
;;    (setf (white s) (xlib:screen-white-pixel (screen s)))
;;		(setf (font s) (xlib:open-font (display s) "fixed"))
;;    (setf (window s) (xlib:create-window :parent (xlib:screen-root (screen s))
;;                                    :x 100 :y 100
;;                                    :width width :height height
;;                                    :background (white s)
;;                                    :border (black s) 
;;                                    :border-width 1
;;                                    :colormap (xlib:screen-default-colormap (screen s))
;;                                    :bit-gravity :center
;;                                    :event-mask '(:exposure :button-press)))
;;    (setf (gcontext s) (xlib:create-gcontext :drawable (window s) :background (black s) :foreground (white s) :font (font s)))
;;    (xlib:set-wm-properties
;;      (window s)
;;      :name 'hello-world
;;      :icon-name "Hello World" 
;;      :resource-name "Hello World" 
;;      :resource-class 'hello-world
;;      :command (list* 'hello-world (uiop:getenv "DISPLAY"))
;;      :x 100 :y 100 :width width :height height
;;      :min-width width :min-height height
;;      :input :off :initial-state :normal)
;;    (xlib:map-window (window s))
;;    s))
;;
;;(defun close-window (state)
;;  (xlib:close-display (display state)))

;; (
;; (let ((display nil)                     ; ; ; ;
;;       (abort t))                              ; ; ; ;
;;   (unwind-protect                         ; ; ; ;
;;        (progn                                  ; ; ; ;
;;          (setq display (open-display host))      ; ; ; ;
;;          (multiple-value-prog1                   ; ; ; ;
;;              (let* ((screen (display-default-screen display)) ; ; ; ;
;;                     (black (screen-black-pixel screen))     ; ; ; ;
;;                     (white (screen-white-pixel screen))     ; ; ; ;
;;                     (font (open-font display font))         ; ; ; ;
;;                     (border 1)			; Minimum margin around the text ; ; ; ;
;;                     (width (+ (text-width font string) (* 2 border))) ; ; ; ;
;;                     (height (+ (max-char-ascent font) (max-char-descent font) (* 2 border))) ; ; ; ;
;;                     (x (truncate (- (screen-width screen) width) 2)) ; ; ; ;
;;                     (y (truncate (- (screen-height screen) height) 2)) ; ; ; ;
;;                     (window (create-window :parent (screen-root screen) ; ; ; ;
;;                                            :x x :y y :width width :height height   ; ; ; ;
;;                                            :background black                       ; ; ; ;
;;                                            :border white                           ; ; ; ;
;;                                            :border-width 1                         ; ; ; ;
;;                                            :colormap (screen-default-colormap screen) ; ; ; ;
;;                                            :bit-gravity :center                    ; ; ; ;
;;                                            :event-mask '(:exposure :button-press))) ; ; ; ;
;;                     (gcontext (create-gcontext :drawable window ; ; ; ;
;;                                                :background black                       ; ; ; ;
;;                                                :foreground white                       ; ; ; ;
;;                                                :font font)))                           ; ; ; ;
;;                ;; Set window manager hints  ; ; ; ;
;;                (set-wm-properties window               ; ; ; ;
;;                                   :name 'hello-world                      ; ; ; ;
;;                                   :icon-name string                       ; ; ; ;
;;                                   :resource-name string                   ; ; ; ;
;;                                   :resource-class 'hello-world            ; ; ; ;
;;                                   :command (list* 'hello-world host args) ; ; ; ;
;;                                   :x x :y y :width width :height height   ; ; ; ;
;;                                   :min-width width :min-height height     ; ; ; ;
;;                                   :input :off :initial-state :normal)     ; ; ; ;
;;                (map-window window)		; Map the window ; ; ; ;
;;                ;; Handle events             ; ; ; ;
;;                (event-case (display :discard-p t :force-output-p t) ; ; ; ;
;;                  (exposure  ;; Come here on exposure events ; ; ; ;
;;                   (window count)                          ; ; ; ;
;;                   (when (zerop count) ;; Ignore all but the last exposure event ; ; ; ;
;;                     (with-state (window)                    ; ; ; ;
;;                       (let ((x (truncate (- (drawable-width window) width) 2)) ; ; ; ;
;;                             (y (truncate (- (+ (drawable-height window) ; ; ; ;
;;                                                (max-char-ascent font))                 ; ; ; ;
;;                                             (max-char-descent font))                ; ; ; ;
;;                                          2)))                                    ; ; ; ;
;;                         ;; Draw text centered in widnow ; ; ; ;
;;                         (clear-area window)                     ; ; ; ;
;;                         (draw-glyphs window gcontext x y string))) ; ; ; ;
;;                     ;; Returning non-nil causes event-case to exit ; ; ; ;
;;                     nil))                                   ; ; ; ;
;;                  (button-press () t)))  ;; Pressing any mouse-button exits ; ; ; ;
;;            (setq abort nil)))                      ; ; ; ;
;;     ;; Ensure display is closed when done ; ; ; ;
;;     (when display                           ; ; ; ;
;;       (close-display display :abort abort)))))
