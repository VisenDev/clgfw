(defsystem "clgfw"
  :version "0.0.1"
  :author "Robert Wess Burnett"
  :license "Apache-2"
  :description "Common Lisp General Framework for Windowing"
  :depends-on (;; X11
               (:feature :linux "clx")

               ;; WAYLAND
               (:feature :linux "wayflan")
               (:feature :linux "posix-shm")
               (:feature :linux "input-event-codes")
               (:feature :linux "cl-xkb")

               )
  :serial t
  :components (:module "src"
               :components ((:file "common")
                            (:module "linux"
                             :if-feature :linux
                             :components ((:file "x11")
                                          (:file "wayland")
                                          (:file "linux")))
                            (:module "macos"
                             :if-feature (:or :macos :darwin)
                             :components ((:file "clozure" :if-feature :ccl)
                                          (:file "objc" :if-feature (:not :ccl)))
                                     )
                            (:module "web"
                             :if-feature :jscl
                             :components ((:file "web")))
                            (:module "jvm"
                             :if-feature :abcl
                             :components ((:file "jvm"))))))

(defsystem "clgfw/example/hello"
  :depends-on ("clgfw")
  :serial t
  :components ((:module "example" 
                :components ((:file "hello"))))
  :build-operation program-op
  :build-pathname "hello" ;; shell name
  :entry-point "clgfw/example/hello:main" ;; thunk
  )
