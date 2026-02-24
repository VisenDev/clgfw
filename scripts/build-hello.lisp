;;;; THIS IS A BUILD SCRIPT FOR THE REST OF THE PROJECT

;; (declaim (optimize (speed 3) (safety 3)))

(load "scripts/setup.lisp")
(asdf:load-system "cl-xkb")
(asdf:load-system "cffi")
(asdf:load-system "clgfw")
(asdf:make "clgfw/example/hello")
(uiop:quit)
