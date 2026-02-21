;;;; THIS IS A BUILD SCRIPT FOR THE REST OF THE PROJECT

(declaim (optimize (speed 3) (safety 0)))

(unless (find-package 'asdf)
  (load "./vendored/asdf/build/asdf.lisp"))

(asdf:initialize-source-registry
 `(:source-registry
   (:tree ,(uiop:getcwd))
   :ignore-inherited-configuration))

(asdf:load-system "clgfw")
(asdf:make "clgfw/example/hello")
(uiop:quit)
