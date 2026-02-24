(when (find-package 'cl-charms/low-level)
  (delete-package 'cl-charms/low-level))

;; (unless (find-package 'asdf)
;;   (load "./vendored/asdf/build/asdf.lisp"))

(asdf:initialize-source-registry
 `(:source-registry
   (:tree ,(uiop:getcwd))
   :ignore-inherited-configuration))
