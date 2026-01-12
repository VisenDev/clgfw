(in-package #:clgfw)

#-abcl
(error "The JVM windowing backend requires abcl")

(require 'java)
(require 'jss)

(defclass ctx/jvm ()
  ((frame :accessor frame)
   canvas
   graphics
   )
)

(defclass jvm-canvas ()
  (foo)
  (:metaclass java:java-class))


(defparameter *my-canvas*
  (java:jnew-runtime-class
   "MyCanvas"
   :superclass "java.awt.Canvas"
   :annotations `("@Override")
   :methods `(("paint"
               :void ("java.awt.Graphics")
               (lambda (this graphics)
                 (format t "Setting color...~%")
                 (let ((method (java:jmethod graphics "setColor") ))
                   (java:jcall method graphics (java:jfield "java.awt.Color" "RED"))
                   (format t "Drawing rect...~%"))
                 )))))

;; (defparameter *test*
;;   (#1"public class MyCanvas extends java.awt.Canvas {

;;       public void paint(java.awt.Graphics g) {
;;            g.drawRect(20, 20, 20, 20);
;;       }
;; }
;; ")
;;  )


(defmethod close-window ((ctx ctx/jvm))
  (with-slots (frame) ctx
    (#"dispose" frame))
  )

(defun init-window/jvm (width height title)
  (let ((ctx (make-instance 'ctx/jvm)))
    (with-slots (frame canvas graphics) ctx
      (setf frame (jss:new 'Jframe title))
      (setf canvas (jss:new *my-canvas*))
      (#"setSize" canvas width height)
      (#"add" frame canvas)
      (#"pack" frame)
      (#"setVisible" frame java:+TRUE+)
      ;;      (#"createBufferStrategy" canvas 2)

      ;;test drawing...
                                        ;      (setf graphics (#"getGraphics" canvas))
                                        ;      (#"drawRect" graphics 100 100 20 20)
      )
    ctx)
  )
