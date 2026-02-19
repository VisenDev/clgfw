;;;; Copyright 2026 Robert Wess Burnett
;;;; 
;;;; Licensed under the Apache License, Version 2.0 (the "License");
;;;; you may not use this file except in compliance with the License.
;;;; You may obtain a copy of the License at
;;;; 
;;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;; 
;;;; Unless required by applicable law or agreed to in writing, software
;;;; distributed under the License is distributed on an "AS IS" BASIS,
;;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;;; See the License for the specific language governing permissions and
;;;; limitations under the License.

(defpackage #:clgfw/backend/x11
  (:use #:cl #:alexandria)
  (:export #:backend/x11))
(in-package #:clgfw/backend/x11)

(clgfw:register-backend 'backend/x11 clgfw:+priority-secondary+ t)

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
   (preferred-text-height :accessor preferred-text-height :initform 25
                          :documentation "At what size should text be drawn")
   (back-buffer-canvas :accessor back-buffer-canvas)
   (window-picture :accessor window-picture)
   ))

(defmethod clgfw:backend-window-should-close-p ((ctx backend/x11))
  (not (window-should-keep-running ctx)))

(defclass canvas/x11 ()
  ((pixmap :accessor pixmap)
   (picture :accessor picture)))

(defun create-canvas (ctx w h drawable)
    (let ((result (make-instance 'canvas/x11)))
      (with-slots (pixmap picture) result
        (setf pixmap (xlib:create-pixmap :width (ceiling w)
                                         :height (ceiling h)
                                         :depth 32
                                         :drawable drawable))


        (setf picture
              (xlib:render-create-picture
               pixmap
               :format (xlib:find-standard-picture-format
                        (display ctx)
                        :argb32)))
        (xlib:render-fill-rectangle
         picture
         :src
         #(0 0 0 0)
         0 0
         (ceiling w)
         (ceiling h))
        )

      (return-from create-canvas result)))

(defmethod clgfw:backend-create-canvas ((ctx backend/x11) w h)
  (create-canvas ctx w h (pixmap (back-buffer-canvas ctx))))

(defmethod clgfw:backend-destroy-canvas ((ctx backend/x11) (canvas canvas/x11))
  (xlib:render-free-picture (picture canvas))
  (xlib:free-pixmap (pixmap canvas))
  (setf (pixmap canvas) nil)
  (setf (picture canvas) nil))

(defmethod clgfw:backend-draw-rectangle-on-canvas ((ctx backend/x11) (canvas canvas/x11)
                                                   x y w h color)

  (let ((buffer (make-array 4 :element-type '(unsigned-byte 16) :initial-element 0))
        (col (clgfw:color-premultiply-alpha color)))
    (declare (dynamic-extent buffer)
             (optimize (speed 3)))
    (setf (aref buffer 0) (ash (clgfw:color-r col) 8))
    (setf (aref buffer 1) (ash (clgfw:color-g col) 8))
    (setf (aref buffer 2) (ash (clgfw:color-b col) 8))
    (setf (aref buffer 3) (ash (clgfw:color-a col) 8))
    (with-slots (pixmap picture) canvas
      (xlib:render-fill-rectangle picture :over buffer x y w h))))

(defmethod clgfw:backend-init-window ((ctx backend/x11) width height title callback-handler-instance)
  "Initialize the x11 window and return the created ctx"
  (setf (handler ctx) callback-handler-instance)
  
  (with-slots (black white font display screen window gcontext colormap window-picture) ctx
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

    (setf window-picture
          (xlib:render-create-picture
           window
           :format (xlib:find-window-picture-format window)))

    (setf (back-buffer-canvas ctx) (create-canvas ctx width height window))
    
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

;; (declaim (ftype (function (backend/x11 clgfw:color) t) convert-to-x11-color))
(defun convert-to-x11-color (ctx color)
  "Computes a hash for the color to see if it is in the colormap already, otherwise, 
allocates the color"
  (declare (optimize (speed 3) (safety 1)))
  (when-let (x11-color (gethash color (color-cache ctx)))
    (return-from convert-to-x11-color x11-color))
  (format t "Allocating missing xlib color ~x :(~%" color)
  (setf (gethash color (color-cache ctx))
        (xlib:alloc-color (colormap ctx)
                          (xlib:make-color 
                           :red (the float (/ (clgfw:color-r color) 256f0))  
                           :green (the float (/ (clgfw:color-g color) 256f0))
                           :blue (the float (/ (clgfw:color-b color) 256f0)))))
  (convert-to-x11-color ctx color))

(defun back-buffer-ensure-correct-size (ctx)
  (with-slots (window back-buffer-canvas) ctx
    (with-slots (pixmap picture) back-buffer-canvas
      (when
          (or (> (xlib:drawable-width window) (xlib:drawable-width pixmap))
              (> (xlib:drawable-height window) (xlib:drawable-height pixmap)))
        (clgfw:backend-destroy-canvas ctx back-buffer-canvas)
        (setf back-buffer-canvas
              (create-canvas ctx
                             (xlib:drawable-width window)
                             (xlib:drawable-height window)
                             window))))))

(defmethod clgfw:backend-draw-rectangle ((ctx backend/x11) x y width height color)
  (when (clgfw:color-invisible-p color)
    (return-from clgfw:backend-draw-rectangle))
  (back-buffer-ensure-correct-size ctx)
  (clgfw:backend-draw-rectangle-on-canvas ctx (back-buffer-canvas ctx)
                                          x y width height color)
  ;; ()
  ;; (setf (xlib:gcontext-foreground (gcontext ctx)) (convert-to-x11-color ctx color))
  ;; (xlib:draw-rectangle (window ctx) (gcontext ctx) x y width height t)
  )


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
  (if-let (font (find-closest-xserver-font (display ctx) (round text-height)))
               (setf (xlib:gcontext-font (gcontext ctx)) font)
               (setf (xlib:gcontext-font (gcontext ctx))
                     (first (xlib:list-font-names (display ctx) "*"))))

  (print (xlib:gcontext-font (gcontext ctx))))


(defmethod clgfw:backend-draw-text ((ctx backend/x11) x y color text)
  (clgfw/bdf:draw-string ctx clgfw/bdf:*fonts* x y (preferred-text-height ctx) color text))

;; (defmethod clgfw:backend-draw-text ((ctx backend/x11) x y color text)
;;   (with-slots (gcontext display) ctx
;;     (setf (xlib:gcontext-foreground gcontext) (convert-to-x11-color ctx color))
;;     (xlib:draw-glyphs (window ctx) (gcontext ctx) x (+ y 15) text)))


(defmethod clgfw:backend-begin-drawing ((ctx backend/x11))
  (clgfw:backend-check-for-input ctx))

(defun convert-keycode (ctx code)
  "Converts an X11 keycode to a list of clgfw keys"
  (let* ((index (xlib:default-keysym-index (display ctx) code 0))
         (sym (xlib:keycode->keysym (display ctx) code index))
         (key
           (typecase sym
             (integer (clgfw:char->key (code-char sym)))
             (character (clgfw:char->key sym))
             (symbol (assert (typep sym 'clgfw:key)) (list sym))
             (t (error "unsupported type ~a" sym))
             )))
    key))

(defmethod clgfw:backend-check-for-input ((ctx backend/x11))
  (clgfw::callback-on-window-resize
   (handler ctx)
   (xlib:drawable-width (window ctx))
   (xlib:drawable-height (window ctx)))

  (multiple-value-bind (x y) (xlib:pointer-position (window ctx))
    (clgfw::callback-on-mouse-move (handler ctx) x y))
  (loop :while (xlib:event-listen (display ctx))
        :do
           (xlib:event-case ((display ctx))
             ;; (:resize-request (width height)
             ;;                  (format t "Window resized to ~a/~a~%" width height)
             ;;                  (setf (xlib:drawable-height (window ctx)) height)
             ;;                  (setf (xlib:drawable-width (window ctx)) width))
             (:key-press (code)
                         (when-let (keys (convert-keycode ctx code))
                           (dolist (key keys)
                             (clgfw:callback-on-key-down (handler ctx) key)))
                         t)
             (:key-release (code)
                           (when-let (keys (convert-keycode ctx code))
                             (dolist (key keys)
                               (clgfw:callback-on-key-up (handler ctx) key)))
                           t)
             (:button-press (code)
                            (clgfw:callback-on-mouse-down
                             (handler ctx)
                             (ecase code
                               (1 :left)
                               (2 :middle)
                               (3 :right)))
                            t)
             (:button-release (code)
                              (clgfw:callback-on-mouse-up
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
                                (return-from clgfw:backend-check-for-input)
                                )
                              t)
             (:destroy-notify ()
                              (setf (window-should-keep-running ctx) nil)
                              (return-from clgfw:backend-check-for-input))
             (t () t))))

(defmethod clgfw:backend-end-drawing ((ctx backend/x11))
  (let ((canvas (back-buffer-canvas ctx)))
    (xlib:render-composite
     :over (picture canvas) 
     nil
     (window-picture ctx)
     0 0 0 0
     0 0
     (xlib:drawable-width (pixmap canvas))
     (xlib:drawable-height (pixmap canvas))))
  (xlib:display-force-output (display ctx)))

(defmethod clgfw:backend-close-window ((ctx backend/x11))
  (ignore-errors (xlib:close-display (display ctx) :abort nil)))

(defmethod clgfw:backend-draw-canvas ((ctx backend/x11) x y canvas &optional tint)
  (declare (ignore tint))
  ;;TODO handle tint
  (xlib:render-composite
   :over (picture canvas)
   nil
   (picture (back-buffer-canvas ctx))
   0 0 0 0
   (round x) (round y)
   (xlib:drawable-width (pixmap canvas))
   (xlib:drawable-height (pixmap canvas))))

;; (defun image-text ()

;;   (with-window ctx (800 600 "image")
;;     (let ((image (create-image ctx 100 100)))
;;       (draw-rectangle image 10 10 20 20 (make-color 0 :b 200))
;;       (while-running/with-drawing ctx
;;         (draw-image ctx image 100 100)        
;;         ))))
