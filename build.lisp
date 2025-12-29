(declaim (optimize (speed 0) (debug 3) (safety 3)))
;(sb-ext:restrict-compiler-policy 'debug 3)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package 'asdf)
    (load "./3rdparty/asdf/build/asdf.lisp")))

;; Force ASDF to look here for systems.
(asdf:initialize-source-registry
  `(:source-registry
     (:directory ,(uiop:getcwd))
     (:directory ,(merge-pathnames (uiop:getcwd) #p"3rdparty/clx"))
     :inherit-configuration))

(format t "--- LOADING SYSTEM ---~%")

(asdf:load-system :clx)
(asdf:load-system :clgfw)
;;;;(load #p"clgfw.lisp")
;;
;;(clgfw:init-window 100 100 "hello")
;;(loop (clgfw:begin-drawing))
