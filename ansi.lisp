;;;; Ansi Terminal Backend

(defpackage #:clgfw/backend/ansi
  (:use #:cl)
  (:export #:backend/ansi))
(in-package #:clgfw/backend/ansi)

(push 'backend/ansi clgfw:*backends*)

(defun esc (control-string &rest args)
  "Construct ansi terminal escape code"
  (apply #'format *standard-output*
         (format nil "~a[~a" #\Esc control-string) args))

(defparameter *coordinate-downscale-factor* 1/10
  "This parameter controls how coordinates are adjusted when using the 
   ansi terminal backend. Because the workable area of an ansi terminal 
   is much more limited than other backends, it makes sense to automatically
   convert coordinates meant for a larger screen (ie 1000+ units wide) into
   coordinates that make more sense for a terminal only ~100 or so units wide"
  )

(defun reset-cursor () (esc "1;1H"))
(defun clear-screen () (esc "2J"))
(defun clear-line () (esc "2K"))
(defun reset-colors () (esc "0m"))
(defun hide-cursor () (esc "?25l"))
(defun show-cursor () (esc "?25h"))
(defun save-screen ()  (esc "?47h"))
(defun restore-screen ()  (esc "?47l"))
(defun report-exact-coordinates () (esc "?1006h"))
(defun mouse-tracking-enable () (esc "?1003h"))
(defun mouse-tracking-disable () (esc "?1003l"))

(defun goto (x y) (esc "~a;~aH" y x))
(defun set-foreground-color (8bitcolor) (esc "38;5;~a;m" 8bitcolor))
(defun set-background-color (8bitcolor) (esc "48;5;~a;m" 8bitcolor))

(defclass backend/ansi ()
  ((handler :accessor handler)))

(defmethod clgfw:backend-init-window ((ctx backend/ansi) width height
                                      title callback-handler-instance)
  (declare (ignore width height title))
  (setf (handler ctx) callback-handler-instance)
  (save-screen)
  (hide-cursor)
  (mouse-tracking-enable)
  ctx)

(defmethod clgfw:backend-close-window ((ctx backend/ansi))
  (reset-cursor)
  (clear-screen)
  (restore-screen)
  (show-cursor))

(defmethod clgfw:backend-get-text-height ((ctx backend/ansi)) 1)

(defun color->8bit (color)
  (let ((r (clgfw:color-r color))
        (g (clgfw:color-g color))
        (b (clgfw:color-b color)))
    (round
     (+ (* (/ (* r 6) 256) 36)
        (* (/ (* g 6) 256) 6)
        (/ (* b 6) 256)))))

(defmethod clgfw:backend-draw-text ((ctx backend/ansi) x y color text)
  (goto x y)
  (set-foreground-color (color->8bit color))
  (format *standard-output* "~a" text))

(defmethod clgfw:backend-begin-drawing ((ctx backend/ansi))
  (declare (ignore ctx)))

(defmethod clgfw:backend-end-drawing ((ctx backend/ansi))
  (declare (ignore ctx))
  (finish-output *standard-output*)
  (sleep 0.3))

(defmethod clgfw:backend-draw-rectangle ((ctx backend/ansi) x y w h color)
  (set-foreground-color (color->8bit color))
  (set-background-color (color->8bit color))
  (loop :for dy :from y :below (+ h y)
        :do
           (goto x dy)
           (loop :repeat x :do (format *standard-output* "#"))))

(defmethod clgfw:backend-window-should-close-p ((ctx backend/ansi))
  ;;TODO make this more advanced
  nil)

;;; For testing
(defun main ()
  (let ((clgfw:*backends* `(backend/ansi))
        (i 0))
    (clgfw:with-window ctx (100 100 "foo")
      (clgfw:while-running ctx
        (incf i)
        (when (> i 100)
          (return-from main))
        (clgfw:with-drawing ctx
          (clgfw:draw-rectangle ctx 0 0 50 10 clgfw:+whitesmoke+)
          (clgfw:draw-text ctx 1 1 clgfw:+moon+ "Hello World")
          )
        )
      ))
  )
