(defsystem "clgfw"
  :version "0.0.1"
  :author "Robert Wess Burnett"
  :license "Apache-2"
  :description "Common Lisp General Framework for Windowing"
  :depends-on (;; X11
               (:feature (:and :linux (:not :abcl)) "clx")

               ;; WAYLAND
               (:feature (:and :linux (:not :abcl)) "wayflan")
               (:feature (:and :linux (:not :abcl)) "posix-shm")
               (:feature (:and :linux (:not :abcl)) "input-event-codes")
               (:feature (:and :linux (:not :abcl)) "cl-xkb")

               ;; COCOA
               (:feature (:or :darwin :macos) "cffi")
               (:feature (:or :darwin :macos) "cffi-libffi")
               (:feature (:or :darwin :macos) "cffi-object")



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
