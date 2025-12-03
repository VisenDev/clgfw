(require :asdf)

;; Force ASDF to only look here for systems.
(asdf:initialize-source-registry `(:source-registry (:tree ,(uiop:getcwd)) :inherit-configuration))

(format t "--- LOADING SYSTEM ---~%")
;(declaim (optimize (speed 3) (debug 1) (safety 1)))
(asdf:load-system :clgfw)

(clgfw:init-window 10 10 "hello")
(clgfw:close-window)
