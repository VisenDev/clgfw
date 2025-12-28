(defsystem :clgfw
  :version "0.0.1"
  :author "Robert Wess Burnett"
  :license "Apache-2"
  :description "Common Lisp General Framework for Windowing"
  :depends-on (:clx)
  :serial nil
  :components ((:file "clgfw"))
  )

(defsystem :clgfw/example
  :depends-on (:clgfw)
  :build-operation program-op
  :build-pathname "main" ;; shell name
  :entry-point "clgfw:init-window" ;; thunk
  )

