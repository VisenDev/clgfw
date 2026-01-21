(defsystem "clgfw"
  :version "0.0.1"
  :author "Robert Wess Burnett"
  :license "Apache-2"
  :description "Common Lisp General Framework for Windowing"
  :depends-on (;; X11
               #-abcl(:feature :linux "clx")

               ;; WAYLAND
               #-abcl(:feature :linux "wayflan")
               #-abcl(:feature :linux "posix-shm")
               #-abcl(:feature :linux "input-event-codes")
               #-abcl(:feature :linux "cl-xkb")
               )
  :serial t
  :components ((:file "package")
               (:file "common")
               #-abcl(:file "x11" :if-feature :linux)
               #-abcl(:file "wayland" :if-feature :linux)
               #-abcl(:file "linux" :if-feature :linux)
               #+abcl(:file "jvm")
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
