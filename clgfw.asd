(defsystem "clgfw"
  :version "0.0.1"
  :author "Robert Wess Burnett"
  :license "Apache-2"
  :description "Common Lisp General Framework for Windowing"
  :depends-on ((:feature :linux "clx")
               (:feature :linux "wayflan"))
  :serial t
  :components ((:file "package")
               (:file "common")
               (:file "x11" :if-feature :linux)
             ;;WIP  (:file "wayland" :if-feature :linux)
               (:file "linux" :if-feature :linux)
               ))

(defsystem "clgfw/example/hello"
  :depends-on ("clgfw")
  :serial t
  :components ((:module "example" 
                :components ((:file "hello"))))
  :build-operation program-op
  :build-pathname "hello" ;; shell name
  :entry-point "clgfw/example/hello:main" ;; thunk
  )
