(in-package #:clgfw)

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

(defmacro while-running (state &body body)
  `(loop :while (window-should-keep-running ,state)
         :do ,@body))

(defmacro while-running/with-drawing (state &body body)
  `(while-running ,state
    (with-drawing ,state ,@body))
  )

