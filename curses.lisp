(defpackage #:clgfw/backend/curses
  (:use #:cl #:alexandria)
  (:export #:backend/curses))
(in-package #:clgfw/backend/curses)

(push 'backend/curses clgfw:*backends*)

(defclass backend/curses ()
  ((handler :accessor handler)
   (win :accessor win)
   (next-color-code :initform 16 :accessor next-color-code)
   (color-cache :accessor color-cache
                :initform (make-hash-table))))

(defun hash-fg-bg (fg bg)
  (logior (ash (abs fg) 16) (abs bg)))

(defun ensure-color-pair (ctx fg bg)
  (with-slots (color-cache next-color-code) ctx
    (let ((hash (hash-fg-bg fg bg)))
      (if-let (code (gethash hash color-cache))
        code
        (progn
          (incf next-color-code)
          (charms/ll:init-pair next-color-code fg bg)
          (setf (gethash hash color-cache) next-color-code)
          (gethash hash color-cache))))))

(defun adj (coordinate)
  "adjust coordinates meant for a normal screen to terminal coordinates"
  (max 0 (floor (* coordinate 1/10))))

;; (defun color->8bit (color)
;;   (let ((r (clgfw:color-r color))
;;         (g (clgfw:color-g color))
;;         (b (clgfw:color-b color)))
;;     (round
;;      (+ (* (/ (* r 6) 256) 36)
;;         (* (/ (* g 6) 256) 6)
;;         (/ (* b 6) 256)))))

(defun color->8bit (color)
  (flet ((scale (v)
           (min 5 (floor (* v 6) 256))))
    (+ 16
       (* 36 (scale (clgfw:color-r color)))
       (* 6  (scale (clgfw:color-g color)))
       (scale (clgfw:color-b color)))))


(defmethod clgfw:backend-init-window ((ctx backend/curses)
                                      width height title
                                      callback-handler-instance)
  (setf (handler ctx) callback-handler-instance)
  (setf (win ctx) (charms:initialize))
  (charms/ll:start-color)
  (charms/ll:use-default-colors)
  (charms:disable-echoing)
  (charms:enable-raw-input :interpret-control-characters t)
  (charms:enable-non-blocking-mode (win ctx))
  (charms/ll:keypad (charms::window-pointer (win ctx)) charms/ll:TRUE)
  (charms/ll:mousemask (logior charms/ll:ALL_MOUSE_EVENTS charms/ll:REPORT_MOUSE_POSITION))
 
  ctx)

(defmethod clgfw:backend-set-preferred-text-height ((ctx backend/curses) text-height)
  (declare (ignore ctx text-height)))

(defmethod clgfw:backend-close-window ((ctx backend/curses))
  (charms:finalize))

(defmethod clgfw:backend-begin-drawing ((ctx backend/curses))
  (with-slots (handler win) ctx
    (when-let (ch (charms:get-char win :ignore-error t))
      (if (equal (char-code ch) charms/ll:KEY_MOUSE)

          ;;then
          (handler-case
           (multiple-value-bind (bstate x y z id) (charms/ll:getmouse)
             (declare (ignore z id))
             (clgfw:callback-on-mouse-move handler (* 10 x) (* 10 y))
             (when (logand charms/ll:BUTTON1_PRESSED bstate)
               (clgfw:callback-on-mouse-down handler :left))
             (when (logand charms/ll:BUTTON2_PRESSED bstate)
               (clgfw:callback-on-mouse-down handler :middle))
             (when (logand charms/ll:BUTTON3_PRESSED bstate)
               (clgfw:callback-on-mouse-down handler :right))
             (when (logand charms/ll:BUTTON1_RELEASED bstate)
               (clgfw:callback-on-mouse-up handler :left))
             (when (logand charms/ll:BUTTON2_RELEASED bstate)
               (clgfw:callback-on-mouse-up handler :middle))
             (when (logand charms/ll:BUTTON3_RELEASED bstate)
               (clgfw:callback-on-mouse-up handler :right)))

            (error (err)
              (format t "Error: ~a" err)))
          
          ;;else
          (when-let (key (clgfw:char->key ch))
            (clgfw:callback-on-key-down (handler ctx) key)))))
  
  (multiple-value-bind (width height)
      (charms:window-dimensions (win ctx))
    (clgfw:callback-on-window-resize (handler ctx) (* 10 width) (* 10 height))))



(defmethod clgfw:backend-draw-rectangle ((ctx backend/curses) x y w h color)
  (charms/ll:attron (charms/ll:color-pair
                     (ensure-color-pair
                      ctx
                      (color->8bit color)
                      (color->8bit color))))
  (loop
    :for dx :from (adj x) :below (adj (+ x w))
    :do
       (loop
         :for dy :from (adj y) :below (adj ( + y h))
         :do (ignore-errors (charms:write-char-at-point (win ctx) #\. dx dy)))))

(defmethod clgfw:backend-draw-text ((ctx backend/curses) x y color text)
  ;; (charms/ll:init-color 0
  ;;                       (clgfw:color-r color)
  ;;                       (clgfw:color-g color)
  ;;                       (clgfw:color-b color))

  ;; (charms/ll:init-pair 0 1 2)

  (charms/ll:attron (charms/ll:color-pair
                     (ensure-color-pair
                      ctx
                      (color->8bit color)
                      -1)))
  (charms:write-string-at-point (win ctx) text (adj x) (adj y)))

(defmethod clgfw:backend-window-should-close-p ((ctx backend/curses))
  (declare (ignore ctx))
  nil)

(defmethod clgfw:backend-end-drawing ((ctx backend/curses))
  (charms:refresh-window (win ctx))
  (sleep 0.01))

;; (defun paint ()
;;   "Paint an asterisk at the cursor, or erase the one already painted."
;;   ;; We don't want to move the cursor when we paint.
;;   (charms:with-restored-cursor (win ctx)
;;     (charms:write-char-at-cursor
;;      (win ctx)
;;      (if (char/= #\Space (charms:char-at-cursor (win ctx)))
;;          #\Space
;;          #\*))))

;; ;;; Main driver

;; (defun main ()
;;   "Start the timer program."
;;   (charms:with-curses ()
;;     (charms:disable-echoing)
;;     (charms:enable-raw-input :interpret-control-characters t)
;;     (charms:enable-non-blocking-mode (win ctx))

;;     (loop :named driver-loop
;;           :with x := 0                  ; Cursor X coordinate
;;           :with y := 0                  ; Cursor Y coordinate
;;           :for c := (charms:get-char (win ctx)
;;                                      :ignore-error t)
;;           :do (progn
;;                 ;; Refresh the window
;;                 (charms:refresh-window (win ctx))

;;                 ;; Process input
;;                 (case c
;;                   ((nil) nil)
;;                   ((#\w) (decf y))
;;                   ((#\a) (decf x))
;;                   ((#\s) (incf y))
;;                   ((#\d) (incf x))
;;                   ((#\Space) (paint))
;;                   ((#\q #\Q) (return-from driver-loop)))

;;                 ;; Normalize the cursor coordinates
;;                 (multiple-value-bind (width height)
;;                     (charms:window-dimensions (win ctx))
;;                   (setf x (mod x width)
;;                         y (mod y height)))

;;                 ;; Move the cursor to the new location
;;                 (charms:move-cursor (win ctx) x y)))))
