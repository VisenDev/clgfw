(defsystem "clgfw"
  :version "0.0.1"
  :author "Robert Wess Burnett"
  :license "Apache-2"
  :description "Common Lisp General Framework for Windowing"
  :depends-on ("local-time"

               ;; X11
               (:feature (:and :linux (:not :abcl)) "clx")

               ;; WAYLAND
               (:feature (:and :linux (:not :abcl)) "wayflan")
               (:feature (:and :linux (:not :abcl)) "posix-shm")
               (:feature (:and :linux (:not :abcl)) "input-event-codes")
               (:feature (:and :linux (:not :abcl)) "cl-xkb")

               ;; COCOA
               ;; (:feature (:and (:or :darwin :macos) (:not :abcl)) "cffi")
               ;; (:feature (:and (:or :darwin :macos) (:not :abcl)) "cffi-libffi")
               ;; (:feature (:and (:or :darwin :macos) (:not :abcl)) "cffi-object")

               )
  :serial t
  :components ((:file "package")
               (:file "common")
               (:file "fps")
               (:file "x11" :if-feature (:and :linux (:not :abcl)))
               (:file "wayland" :if-feature (:and :linux (:not :abcl)))
               (:file "linux" :if-feature (:and :linux (:not :abcl)))
               (:file "jvm" :if-feature :abcl)
               ))

(defsystem "clgfw/bdf"
  :author "Robert Wess Burnett"
  :license "Apache-2"
  :description "Bitmap font parser"
  :depends-on ("alexandria")
  :components ((:module "bdf"
                :components
                ((:file "bdf.lisp")))))

(defsystem "clgfw/example/hello"
  :depends-on ("clgfw")
  :description "Basic example"
  :serial t
  :components ((:module "example" 
                :components ((:file "hello"))))
  :build-operation program-op
  :build-pathname "hello"                 ;; shell name
  :entry-point "clgfw/example/hello:main" ;; thunk
  )
