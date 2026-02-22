(defpackage #:clgfw/example/hello
  (:use #:cl)
  (:export #:main))
(in-package #:clgfw/example/hello)

(defun main ()
  "Example main function"
  (prog ((x 10)
        (y 10)
        (sz 30)
        (delta-x 60)
        (delta-y 60))
     (clgfw:with-window ctx (800 600 "Hello")
      
       (clgfw:set-preferred-text-height ctx sz)
       (clgfw:set-redraw-frequency ctx :target-fps 170)

       (clgfw:with-canvas test (ctx 50 50)
         (clgfw:with-drawing-on-canvas (ctx test)
           (clgfw:draw-rectangle ctx 0 0 40 50 clgfw/color:+black+)
           (clgfw:draw-rectangle ctx 0 0 5 50 clgfw/color:+aliceblue+)
           (clgfw:draw-rectangle ctx 45 0 5 50 clgfw/color:+aliceblue+))
        
        
         (clgfw:while-running ctx
           (clgfw:with-drawing ctx
             (let ((w (clgfw:get-window-width ctx))
                   (h (clgfw:get-window-height ctx)))

               (when (clgfw:is-key-pressed ctx :q)
                 (go quit))
              
               (clgfw:draw-rectangle ctx 0 0 w h clgfw/color:+space+)
              
               (clgfw:draw-canvas ctx 100 100 test)
               (clgfw:draw-rectangle ctx (floor x) (floor y) sz sz clgfw/color:+moon+)
               (clgfw:draw-text ctx 10 10 clgfw/color:+white+
                                (format nil "Width: ~a px,   Height: ~a px" w h))
               (clgfw:draw-text ctx 10 40 clgfw/color:+skyblue+ (clgfw:get-fps-string ctx))
               (clgfw:draw-text ctx 10 70 clgfw/color:+red+ "Press 'q' to quit!")
               (clgfw:draw-rectangle ctx
                                     (clgfw:get-mouse-x ctx)
                                     (clgfw:get-mouse-y ctx)
                                     sz sz
                                     (clgfw:make-color 20 200 20 128))


               (incf x (* delta-x (clgfw:get-delta-time ctx)))
               (incf y (* delta-y (clgfw:get-delta-time ctx)))
               (when (or (< x 0) (> x (- w sz)))
                 (setf delta-x (* delta-x -1))
                 (incf delta-x (- 0.005 (random 0.01))))
               (when (or (< y 0) (> y (- h sz)))
                 (setf delta-y (* delta-y -1))
                 (incf delta-y (- 0.005 (random 0.01)))))))))
     quit
     (uiop:quit)))

