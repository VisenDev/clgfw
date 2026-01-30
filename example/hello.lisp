(defpackage #:clgfw/example/hello
  (:use #:cl)
  (:export #:main))
(in-package #:clgfw/example/hello)

(defun main ()
  "Example main function"
  (let ((bg clgfw/color:+space+)
        (fg clgfw/color:+moon+)
        (text-size 50)
        (x 0) (y 0) (sz 60) (delta-x 0.5) (delta-y 0.5))
    (clgfw:with-window ctx (800 600 "Hello")
      (clgfw:while-running/with-drawing ctx

        (let ((w (clgfw:get-window-width ctx))
              (h (clgfw:get-window-height ctx)))

          (when (clgfw:is-key-pressed ctx :q)
            (return-from main))
          
          (clgfw:draw-rectangle ctx 0 0 w h bg)
          (clgfw:draw-rectangle ctx (floor x) (floor y) sz sz fg)
          (clgfw:draw-rectangle ctx (clgfw:get-mouse-x ctx) (clgfw:get-mouse-y ctx) 10 10 fg)
          
          (clgfw:draw-text ctx 10 10 text-size fg
                           (format nil "FPS ~a" (clgfw:get-fps ctx)))
          
          (clgfw:draw-text ctx 10 100 text-size fg "Press 'q' to quit!")
          
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

