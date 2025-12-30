(defpackage #:clgfw/example/hello
  (:use #:cl)
  (:export #:main))
(in-package #:clgfw/example/hello)

(defun main ()
  "Example main function"
  (let ((down (clgfw:make-color :r 100 :g 100 :b 100))
        (up (clgfw:make-color :r 200 :g 34 :b 223)))
    (clgfw:with-window state (100 100 "Hello")
      (clgfw:while-running/with-drawing state
        (clgfw:draw-rectangle state
                              (clgfw:get-mouse-x state) (clgfw:get-mouse-y state)
                              10 10
                              (if (clgfw:is-mouse-button-down state :left)
                                  down
                                  up))))))

