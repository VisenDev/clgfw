(in-package #:clgfw)

#-abcl
(error "The JVM windowing backend requires abcl")

(require 'java)
;;(require 'jss)

(defclass ctx/jvm ()
  ((frame :accessor frame)
   canvas
   buffer-strategy
   graphics
   (window-should-close :initform nil)))

(defun init-window/jvm (width height title)
  (declare (ignorable width height title))
  (let ((result (make-instance 'ctx/jvm)))
    (with-slots (frame canvas buffer-strategy graphics window-should-close) result
      (setf frame (java:jnew (java:jclass '|java.awt.Frame|)))
      (setf canvas (java:jnew (java:jclass '|java.awt.Canvas|)))
      (java:jcall
       (java:jmethod '|java.awt.Frame| '|setSize| '|int| '|int|)
       frame width height)
      (java:jcall
       (java:jmethod '|java.awt.Frame| '|add| '|java.awt.Component|)
       frame canvas)
      (java:jcall
       (java:jmethod '|java.awt.Frame| '|setVisible| '|boolean|)
       frame t)

      ;; the tricky bit
      (java:jnew-runtime-class "ClgfwWindowAdapter"
                               :superclass "java.awt.event.WindowAdapter"
                               :methods '(("windowClosing" "void" ("java.awt.event.WindowEvent")
                                                           (lambda (event)
                                                             (declare (ignore event))
                                                             (setf window-should-close t)
                                                             (format t "Window should close~%")))))

      (java:jcall
       (java:jmethod '|java.awt.Canvas| '|createBufferStrategy| '|int|)
       canvas 2)
      (setf buffer-strategy (java:jcall
                             (java:jmethod '|java.awt.Canvas| '|getBufferStrategy|)
                             canvas))
      )
    (return-from init-window/jvm result)
    )
  )

(defmethod begin-drawing ((ctx ctx/jvm))
  (with-slots (graphics buffer-strategy canvas) ctx
    (setf graphics (java:jcall
                    (java:jmethod '|java.awt.image.BufferStrategy| '|getDrawGraphics|)
                    buffer-strategy))
    
    )
  )

(defmethod end-drawing ((ctx ctx/jvm))
  (with-slots (graphics buffer-strategy) ctx
    (java:jcall
     (java:jmethod '|java.awt.Graphics| '|dispose|)
     graphics)
    (java:jcall
     (java:jmethod '|java.awt.image.BufferStrategy| '|show|)
     buffer-strategy)
    )
  )

(defmethod draw-rectangle ((ctx ctx/jvm) x y width height color)
  (declare (ignorable color))
  (with-slots (graphics) ctx
    (java:jcall
     (java:jmethod '|java.awt.Graphics| '|setColor| '|java.awt.Color|)
     graphics
     (java:jfield '|java.awt.Color| "RED" ))
    (java:jcall
     (java:jmethod '|java.awt.Graphics| '|drawRect| '|int| '|int| '|int| '|int|)
     graphics x y width height)
    )
  )

(defmethod close-window ((ctx ctx/jvm))
  (with-slots (frame) ctx
    (java:jcall
     (java:jmethod '|java.awt.Frame| '|dispose|)
     frame)
    )
  )

(defun test-main/jvm ()
  (let ((ctx (init-window/jvm 600 400 "hello")))
    (loop :while (not (slot-value ctx 'window-should-close))
          :do (begin-drawing ctx)
              (draw-rectangle ctx 10 10 100 100 t)
              (end-drawing ctx)
          )
    (close-window ctx)
    )
  )


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


;; (defmethod close-window ((ctx ctx/jvm))
;;   (with-slots (frame) ctx
;;     (#"dispose" frame))
;;   )

;; (defun init-window/jvm (width height title)
;;   (let ((ctx (make-instance 'ctx/jvm)))
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
 
