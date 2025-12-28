(defpackage #:clgfw/x11
  (:use #:cl)
  (:export #:init-window
           #:close-window
           #:window-should-close
           #:begin-drawing
           #:end-drawing
           #:draw-pixel))
(in-package #:clgfw/x11)

(defclass state ()
  ((black :accessor black)
   (white :accessor white)
   (font :accessor font)
   (display :accessor display)
   (screen :accessor screen)
   (window :accessor window)
   (gcontext :accessor gcontext)))

(defun init-window (width height title &aux state)
  (declare (ignorable width height title))
  (setf state (make-instance 'state))
  (with-slots (black white font display screen window gcontext) state
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
                  :event-mask (xlib:make-event-mask :leave-window :exposure)))
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
    (xlib:map-window window)
    state
    ;; (loop
    ;;   (xlib:process-event display
    ;;                       :handler (lambda (&rest args)
    ;;                                  (format t "args: ~a~%" args)
    ;;                                  t)))
    )
)

;; (defun begin-drawing (win)
;;   ()
;;   (xlib:event-case display
;;     ())
;;   (xlib:process-event display
;;                       :handler (lambda (&rest args)
;;                                  (format t "args: ~a~%" args)
;;                                  t))
;;   (xlib:prc)
;;   )

(defun close-window (win)
  (xlib:close-display (display win) :abort nil))

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
