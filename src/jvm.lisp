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


(in-package #:clgfw)

#-abcl
(error "The JVM windowing backend requires abcl")

(require 'java)

(defclass backend/jvm (fps-manager)
  ((frame :accessor frame)
   (callback-handler-instance :accessor handler)
   canvas
   buffer-strategy
   graphics
   (window-should-keep-running :initform t :accessor window-should-keep-running)))

(clgfw:register-backend 'backend/jvm clgfw:+priority-native+)

(defmethod backend-window-should-close-p ((ctx backend/jvm))
  (not (window-should-keep-running ctx))
  )

;; MouseInfo.getPointerInfo().getLocation()
(defun get-mouse-x/java (ctx)
  (declare (ignorable ctx))
  (java:jfield '|x| 
               (java:jcall
                (java:jmethod '|java.awt.PointerInfo| '|getLocation|)
                (java:jcall
                 #.(java:jmethod '|java.awt.MouseInfo| '|getPointerInfo|)
                 '|java.awt.MouseInfo|
                 ))))

(defmethod get-mouse-y/java (ctx)
  (declare (ignorable ctx))
  (java:jfield '|y| 
               (java:jcall
                (java:jmethod '|java.awt.PointerInfo| '|getLocation|)
                (java:jcall
                 #.(java:jmethod '|java.awt.MouseInfo| '|getPointerInfo|)
                 '|java.awt.MouseInfo|
                 ))))

(defmethod backend-init-window ((ctx backend/jvm) width height title callback-handler-instance)
  (declare (ignorable width height title))
  (setf (handler ctx) callback-handler-instance)
  (with-slots (frame canvas buffer-strategy graphics window-should-keep-running) ctx
    (setf frame (java:jnew (java:jclass '|java.awt.Frame|)))
    (setf canvas (java:jnew (java:jclass '|java.awt.Canvas|)))
    (java:jcall
     #.(java:jmethod '|java.awt.Frame| '|setSize| '|int| '|int|)
     frame width height)
    (java:jcall
     #.(java:jmethod '|java.awt.Frame| '|add| '|java.awt.Component|)
     frame canvas)
    (java:jcall
     #.(java:jmethod '|java.awt.Frame| '|setVisible| '|boolean|)
     frame t)
    (java:jcall
     #.(java:jmethod '|java.awt.Frame| '|setTitle| '|java.lang.String|)
     frame title)

    ;; the tricky bit
    (let ((clgfw-window-adapter
            (java:jnew-runtime-class "ClgfwWindowAdapter"
                                     :superclass "java.awt.event.WindowAdapter"
                                     :access-flags '(:public)
                                     :methods `(("windowClosing" :void ("java.awt.event.WindowEvent")
                                                                 ,(lambda (this event)
                                                                    (declare (ignore this event))
                                                                    (setf window-should-keep-running nil)
                                                                    )
                                                                 ))
                                     )))
      (java:jcall
       #.(java:jmethod '|java.awt.Frame| '|addWindowListener| '|java.awt.event.WindowListener|)
       frame (java:jnew clgfw-window-adapter)))

    (java:jcall
     #.(java:jmethod '|java.awt.Canvas| '|createBufferStrategy| '|int|)
     canvas 2)
    (setf buffer-strategy (java:jcall
                           #.(java:jmethod '|java.awt.Canvas| '|getBufferStrategy|)
                           canvas))

    (java:jcall
     #.(java:jmethod '|java.awt.Canvas| '|setIgnoreRepaint| '|boolean|)
     canvas t)

    (java:jcall
     #.(java:jmethod '|java.awt.Frame| '|setIgnoreRepaint| '|boolean|)
     frame t)

    )
  (return-from backend-init-window ctx))

(defmethod backend-get-window-width ((ctx backend/jvm))
  (java:jcall
   #.(java:jmethod '|java.awt.Frame| '|getWidth|)
   (frame ctx)))

(defmethod backend-get-window-height ((ctx backend/jvm))
  (java:jcall
   #.(java:jmethod '|java.awt.Frame| '|getHeight|)
   (frame ctx)))

(defmethod backend-begin-drawing ((ctx backend/jvm))
  (with-slots (graphics buffer-strategy canvas) ctx
    (setf graphics (java:jcall
                    #.(java:jmethod '|java.awt.image.BufferStrategy| '|getDrawGraphics|)
                    buffer-strategy))))

(defmethod backend-end-drawing ((ctx backend/jvm))
  (with-slots (graphics buffer-strategy) ctx
    (java:jcall
     #.(java:jmethod '|java.awt.Graphics| '|dispose|)
     graphics)
    
    (java:jcall
     #.(java:jmethod '|java.awt.image.BufferStrategy| '|show|)
     buffer-strategy)
    
    (java:jcall
     #.(java:jmethod '|java.awt.Toolkit| '|sync|)
     (java:jcall
      #.(java:jmethod '|java.awt.Toolkit| '|getDefaultToolkit|)
      (java:jclass '|java.awt.Toolkit|)))))

(defun get-cached-color (color)
  (java:jnew
   (java:jclass '|java.awt.Color|)
   (color-r color)
   (color-g color)
   (color-b color))
  ;; (if (cached color)
  ;;     (cached color)
  ;;     (progn
  ;;       (setf (cached color)
  ;;             (java:jnew
  ;;              (java:jclass '|java.awt.Color|)
  ;;              (color-r color)
  ;;              (color-g color)
  ;;              (color-b color)))
  ;;       (cached color)))
  )

(defmethod backend-draw-rectangle ((ctx backend/jvm) x y width height color)
  (declare (ignorable color))
  (with-slots (graphics) ctx
    (java:jcall
     #.(java:jmethod '|java.awt.Graphics| '|setColor| '|java.awt.Color|)
     graphics
     (get-cached-color color))
    
    (java:jcall
     #.(java:jmethod '|java.awt.Graphics| '|fillRect| '|int| '|int| '|int| '|int|)
     graphics
     (round x) (round y)
     (round width) (round height))
    )
  )

(defmethod backend-draw-text ((ctx backend/jvm) x y text-height color text)
  (with-slots (graphics) ctx
    (java:jcall
     #.(java:jmethod '|java.awt.Graphics| '|setColor| '|java.awt.Color|)
     graphics
     (get-cached-color color)
     )

    ;;TODO cache this font
    (java:jcall
     #.(java:jmethod '|java.awt.Graphics| '|setFont| '|java.awt.Font|)
     graphics
     (java:jcall
      #.(java:jmethod '|java.awt.Font| '|deriveFont| '|float|)
      (java:jcall
       #.(java:jmethod '|java.awt.Graphics| '|getFont|)
       graphics)
      text-height))
    
    (java:jcall
     #.(java:jmethod '|java.awt.Graphics| '|drawString| '|java.lang.String| '|int| '|int|)
     graphics
     text
     (round x) (round (+ y text-height)))    
    ))

(defmethod backend-close-window ((ctx backend/jvm))
  (with-slots (frame) ctx
    (java:jcall
     #.(java:jmethod '|java.awt.Frame| '|dispose|)
     frame)
    )
  )

;; (defun test-main/jvm ()
;;   (let ((ctx (init-window/jvm 600 400 "hello"))
;;         (i 0))
;;     (loop :while (window-should-keep-running ctx)
;;           :do (begin-drawing ctx)
;;               (draw-rectangle ctx i i 100 100 t)
;;               (incf i)
;;               (end-drawing ctx)
;;           )
;;     (close-window ctx)
;;     )
;;   )


;; (defclass jvm-canvas ()
;;   (foo)
;;   (:metaclass java:java-class))


;; (defparameter *my-canvas*
;;   (java:jnew-runtime-class
;;    "MyCanvas"
;;    :superclass "java.awt.Canvas"
;;    :annotations `("@Override")
;;    :methods `(("paint"
;;                :void ("java.awt.Graphics")
;;                (lambda (this graphics)
;;                  (format t "Setting color...~%")
;;                  (let ((method (java:jmethod graphics "setColor") ))
;;                    (java:jcall method graphics (java:jfield "java.awt.Color" "RED"))
;;                    (format t "Drawing rect...~%"))
;;                  )))))

;; (defparameter *test*
;;   (#1"public class MyCanvas extends java.awt.Canvas {

;;       public void paint(java.awt.Graphics g) {
;;            g.drawRect(20, 20, 20, 20);
;;       }
;; }
;; ")
;;  )


;; (defmethod close-window ((ctx backend/jvm))
;;   (with-slots (frame) ctx
;;     (#"dispose" frame))
;;   )

;; (defun init-window/jvm (width height title)
;;   (let ((ctx (make-instance 'backend/jvm)))
;;     (with-slots (frame canvas graphics) ctx
;;       (setf frame (jss:new 'Jframe title))
;;       (setf canvas (jss:new *my-canvas*))
;;       (#"setSize" canvas width height)
;;       (#"add" frame canvas)
;;       (#"pack" frame)
;;       (#"setVisible" frame java:+TRUE+)
;;       ;;      (#"createBufferStrategy" canvas 2)

;;       ;;test drawing...
;;                                         ;      (setf graphics (#"getGraphics" canvas))
;;                                         ;      (#"drawRect" graphics 100 100 20 20)
;;       )
;;     ctx)
;;   )
 
