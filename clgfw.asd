(initialize-source-registry `(:source-registry (:tree ,(uiop:getcwd)) :inherit-configuration))

(defsystem #:clgfw
  :version "0.0.1"
  :author "Robert Wess Burnett"
  :license "Apache-2"

  :description "Common Lisp General Framework for Windowing"
  :depends-on (#:wayflan-client #:posix-shm)
  :serial nil
  :components ((:file "clgfw"))
  )
