(declaim (optimize (speed 0) (debug 3) (safety 3)))

(unless (find-package 'asdf)
  (load "./3rdparty/asdf/build/asdf.lisp"))

(asdf:initialize-source-registry
 `(:source-registry
   (:tree ,(uiop:getcwd))
   :ignore-inherited-configuration))

(asdf:load-system "clx")
(asdf:load-system "clgfw")
(asdf:make "clgfw/example")
(uiop:quit)


