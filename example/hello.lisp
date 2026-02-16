(defpackage #:clgfw/example/hello
  (:use #:cl)
  (:export #:main))
(in-package #:clgfw/example/hello)

(defun main ()
  "Example main function"
  (let ((x 0)
        (y 0)
        (sz 30)
        (delta-x 0.2)
        (delta-y 0.2))
    (clgfw:with-window ctx (800 600 "Hello")
      (clgfw:set-preferred-text-height ctx sz)
      (clgfw:with-canvas player-sprite (ctx 800 600)
        (clgfw:with-drawing-on-canvas
          
          )
        
        (clgfw:while-running ctx
          (clgfw:with-drawing ctx
            (let ((w (clgfw:get-window-width ctx))
                  (h (clgfw:get-window-height ctx)))

              (when (clgfw:is-key-pressed ctx :q)
                (return-from main))
              
              (clgfw:draw-rectangle ctx 0 0 w h clgfw:+space+)
              (clgfw:draw-rectangle ctx (floor x) (floor y) sz sz clgfw:+moon+)
              (clgfw:draw-text ctx 10 10 clgfw:+red+ (format nil "|w:~a|h:~a|" w h))
              (clgfw:draw-text ctx 10 100 clgfw:+brown+ "Press 'q' to quit!")
              (clgfw:draw-rectangle
               ctx
               (clgfw:get-mouse-x ctx)
               (clgfw:get-mouse-y ctx)
               sz sz
               clgfw:+red+)
              
              (incf x delta-x)
              (incf y delta-y)
              (when (or (< x 0) (> x (- w sz)))
                (setf delta-x (* delta-x -1))
                (incf delta-x (- 0.005 (random 0.01)))
                )
              (when (or (< y 0) (> y (- h sz)))
                (setf delta-y (* delta-y -1))
                (incf delta-y (- 0.005 (random 0.01)))))))))))

