(defpackage #:clgfw/example/hello
  (:use #:cl)
  (:export #:main))
(in-package #:clgfw/example/hello)

(defun main ()
  "Example main function"
  (let ((bg clgfw:+space+)
        (fg clgfw:+moon+)
        (x 0) (y 0) (sz 60) (delta-x 0.2) (delta-y 0.2))
    (clgfw:with-window ctx (800 600 "Hello")
      (let (;; (img (clgfw:create-sprite ctx 800 800))
            )
        ;; (clgfw:draw-text img 10 100 text-size fg "Press 'q' to quit!")
        ;; (clgfw:set-target-fps)
        (clgfw:set-preferred-text-height ctx 20)
        
        (clgfw:while-running ctx
          (clgfw:with-drawing ctx
            (let ((w (clgfw:get-window-width ctx))
                  (h (clgfw:get-window-height ctx)))

              (when (clgfw:is-key-pressed ctx :q)
                (return-from main))
              
              (clgfw:draw-rectangle ctx 0 0 w h bg)
              (clgfw:draw-rectangle ctx (floor x) (floor y) sz sz fg)

              (clgfw:draw-text ctx 100 10 clgfw:+red+
                               (format nil "FPS ~a" 30 ;; (clgfw:get-fps ctx)
                                       ))
              
              ;; (clgfw:draw-text ctx 10 100 text-size fg "Press 'q' to quit!")
              ;; (clgfw:draw-sprite ctx img 10 100
              ;;                    (clgfw:make-color
              ;;                     (alexandria:clamp (+ 50 (floor (* 255 (/ (clgfw:get-mouse-x ctx)
              ;;                                                              (clgfw:get-window-width ctx)))))
              ;;                                       0 255)
              ;;                     (alexandria:clamp (+ 50 (floor (* 255 (/ (clgfw:get-mouse-y ctx)
              ;;                                                              (clgfw:get-window-height ctx)))))
              ;;                                       0 255)
              ;;                                      255))

              (clgfw:draw-rectangle ctx (- (clgfw:get-mouse-x ctx) 10) (- (clgfw:get-mouse-y ctx) 10)
                                    20 20
                                    (clgfw:make-color 200 100 100 100))
              
              (incf x (* ;; (clgfw:get-delta-time ctx)
                       delta-x))
              (incf y (* ;; (clgfw:get-delta-time ctx)
                       delta-y))
              (when (or (< x 0) (> x (- w sz)))
                (setf delta-x (* delta-x -1))
                (incf delta-x (- 0.005 (random 0.01)))
                )
              (when (or (< y 0) (> y (- h sz)))
                (setf delta-y (* delta-y -1))
                (incf delta-y (- 0.005 (random 0.01)))
                ))))))))

