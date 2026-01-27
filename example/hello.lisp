(defpackage #:clgfw/example/hello
  (:use #:cl)
  (:export #:main))
(in-package #:clgfw/example/hello)

(defun main ()
  "Example main function"
  (let ((bg (clgfw:make-color :r 100 :g 100 :b 100))
        (fg (clgfw:make-color :r 200 :g 34 :b 223))
        (x 0) (y 0) (sz 60) (delta-x 0.1) (delta-y 0.1))
    (clgfw:with-window ctx (800 600 "Hello")

      (clgfw:while-running/with-drawing ctx
        (print (clgfw:get-fps ctx))
        (let ((w (clgfw:get-window-width ctx))
              (h (clgfw:get-window-height ctx)))
          (clgfw:draw-rectangle ctx 0 0 w h bg)
          (clgfw:draw-rectangle ctx x y sz sz fg)
          (incf x delta-x)
          (incf y delta-y)
          (when (or (< x 0) (> x (- w sz)))
            (setf delta-x (* delta-x -1))
            (incf delta-x (- 0.005 (random 0.01)))
            )
          (when (or (< y 0) (> y (- h sz)))
            (setf delta-y (* delta-y -1))
            (incf delta-y (- 0.005 (random 0.01)))
            ))))))

