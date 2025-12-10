(sb-ext:restrict-compiler-policy 'debug 3)

(require :asdf)

;; Force ASDF to look here for systems.
#|(asdf:initialize-source-registry
  `(:source-registry
     (:directory ,(uiop:getcwd))
     (:directory ,(merge-pathnames (uiop:getcwd) #p"3rdparty/clx"))
     :inherit-configuration))|#

(format t "--- LOADING SYSTEM ---~%")
(declaim (optimize (speed 0) (debug 3) (safety 3)))
(asdf:load-system :clx)
(asdf:load-system :clgfw)
;;(load #p"clgfw.lisp")

(clgfw:init-window 100 100 "hello")
(loop (clgfw:begin-drawing))
