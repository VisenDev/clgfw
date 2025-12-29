(defpackage #:clgfw/example/hello
  (:use #:cl)
  (:export #:main))
(in-package #:clgfw/example/hello)

(defun main ()
  "Example main function"
  (clgfw:with-window state (100 100 "Hello")
    (clgfw:while-running/with-drawing state
      (clgfw:draw-rectangle state
                      :x (clgfw:get-mouse-x state) :y (clgfw:get-mouse-y state)
                      :width 10 :height 10
                      :r 200 :g 100 :b 100))))

