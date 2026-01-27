(in-package #:clgfw)

#-abcl
(error "The JVM windowing backend requires abcl")

(require 'java)

(defclass ctx/jvm (fps-manager)
  ((frame :accessor frame)
   canvas
   buffer-strategy
   graphics
   (window-should-keep-running :initform t :accessor window-should-keep-running)))

(defun init-window/jvm (width height title)
  (declare (ignorable width height title))
  (let ((result (make-instance 'ctx/jvm)))
    (with-slots (frame canvas buffer-strategy graphics window-should-keep-running) result
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
      )
    (return-from init-window/jvm result)
    )
  )

(defmethod get-window-width ((ctx ctx/jvm))
  (java:jcall
   #.(java:jmethod '|java.awt.Frame| '|getWidth|)
   (frame ctx)))

(defmethod get-window-height ((ctx ctx/jvm))
  (java:jcall
   #.(java:jmethod '|java.awt.Frame| '|getHeight|)
   (frame ctx)))

(defmethod begin-drawing ((ctx ctx/jvm))
  (with-slots (graphics buffer-strategy canvas) ctx
    (setf graphics (java:jcall
                    #.(java:jmethod '|java.awt.image.BufferStrategy| '|getDrawGraphics|)
                    buffer-strategy))))

(defmethod end-drawing ((ctx ctx/jvm))
  (with-slots (graphics buffer-strategy) ctx
    (java:jcall
     #.(java:jmethod '|java.awt.Graphics| '|dispose|)
     graphics)
    (java:jcall
     #.(java:jmethod '|java.awt.image.BufferStrategy| '|show|)
     buffer-strategy)
    )
  )

(defmethod draw-rectangle ((ctx ctx/jvm) x y width height color)
  (declare (ignorable color))
  (with-slots (graphics) ctx
    (java:jcall
     #.(java:jmethod '|java.awt.Graphics| '|setColor| '|java.awt.Color|)
     graphics
     (java:jnew (java:jclass '|java.awt.Color|) (color-r color) (color-b color) (color-g color)))
    (java:jcall
     #.(java:jmethod '|java.awt.Graphics| '|fillRect| '|int| '|int| '|int| '|int|)
     graphics
     (round x) (round y)
     (round width) (round height))
    )
  )

(defmethod close-window ((ctx ctx/jvm))
  (with-slots (frame) ctx
    (java:jcall
     #.(java:jmethod '|java.awt.Frame| '|dispose|)
     frame)
    )
  )

(defun test-main/jvm ()
  (let ((ctx (init-window/jvm 600 400 "hello"))
        (i 0))
    (loop :while (window-should-keep-running ctx)
          :do (begin-drawing ctx)
              (draw-rectangle ctx i i 100 100 t)
              (incf i)
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
 
