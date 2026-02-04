(defpackage #:clgfw/backend/curses
  (:use #:cl)
  (:export #:backend/curses))
(in-package #:clgfw/backend/curses)

(push 'backend/curses clgfw:*backends*)

(defclass backend/curses ()
  ((handler :accessor handler)
   (win :accessor win)
   (color-cache :accessor color-cache
                :initform (make-hash-table))))

(defun adj (coordinate)
  "adjust coordinates meant for a normal screen to terminal coordinates"
  (max 0 (floor (* coordinate 1/10))))

(defmethod clgfw:backend-init-window ((ctx backend/curses)
                                      width height title
                                      callback-handler-instance)
  (setf (handler ctx) callback-handler-instance)
  (setf (win ctx) (charms:initialize))
  (charms/ll:start-color)
  (charms:disable-echoing)
  (charms:enable-raw-input :interpret-control-characters t)
  (charms:enable-non-blocking-mode (win ctx))
  ctx)

(defmethod clgfw:backend-set-preferred-text-height ((ctx backend/curses) text-height)
  (declare (ignore ctx text-height)))

(defmethod clgfw:backend-close-window ((ctx backend/curses))
  (charms:finalize))

(defmethod clgfw:backend-begin-drawing ((ctx backend/curses))
  (clgfw:when-it (charms:get-char (win ctx) :ignore-error t)
    (clgfw:when-it (clgfw:char->key it)
      (clgfw:callback-on-key-down (handler ctx) it)))
  
  (multiple-value-bind (width height)
      (charms:window-dimensions (win ctx))
    (clgfw:callback-on-window-resize (handler ctx) (* 10 width) (* 10 height))))

(defmethod clgfw:backend-draw-rectangle ((ctx backend/curses) x y w h color)
  ;; (charms/ll:init-color 0 (clgfw:color-r color)
  ;;                       (clgfw:color-g color)
  ;;                       (clgfw:color-b color))
  ;; (charms/ll:init-pair 0 2 30)
  ;; (charms/ll:attron (charms/ll:color-pair 0))
  (loop
    :for dx :from (adj x) :below (adj (+ x w))
    :do
       (loop
         :for dy :from (adj y) :below (adj ( + y h))
         :do (charms:write-char-at-point (win ctx) #\# dx dy))))

(defmethod clgfw:backend-draw-text ((ctx backend/curses) x y color text)
  ;; (charms/ll:init-color 0
  ;;                       (clgfw:color-r color)
  ;;                       (clgfw:color-g color)
  ;;                       (clgfw:color-b color))

  ;; (charms/ll:init-pair 0 1 2)
  ;; (charms/ll:attron (charms/ll:color-pair 0))
  (charms:write-string-at-point (win ctx) text (adj x) (adj y)))

(defmethod clgfw:backend-window-should-close-p ((ctx backend/curses))
  (declare (ignore ctx))
  nil)

(defmethod clgfw:backend-end-drawing ((ctx backend/curses))
  (charms:refresh-window (win ctx))
  (sleep 0.1))

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
