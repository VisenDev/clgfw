(defsystem "clgfw/core"
  :version "0.0.1"
  :author "Robert Wess Burnett"
  :license "Apache-2"
  :description "Common Lisp General Framework for Windowing"
  :depends-on ("local-time" "uiop")
  :serial t
  :components ((:file "package")
               (:file "common")
               (:file "colors")))

(defsystem "clgfw/backend/wayland"
  :depends-on ("clgfw/core" "wayflan" "posix-shm" "input-event-codes" "cl-xkb")
  :components ((:file "wayland")))

(defsystem "clgfw/backend/x11"
  :depends-on ("clgfw/core" "clx")
  :components ((:file "x11")))

(defsystem "clgfw/backend/jvm"
  :depends-on ("clgfw/core")
  :components ((:file "jvm")))

(defsystem "clgfw"
  :version "0.0.1"
  :author "Robert Wess Burnett"
  :license "Apache-2"
  :description "Common Lisp General Framework for Windowing"
  :depends-on ((:feature :abcl "clgfw/backend/jvm")
               (:feature (:or :bsd :linux :unix :macos :macosx :darwin) "clgfw/backend/x11")
               (:feature :linux "clgfw/backend/wayland")))

(defsystem "clgfw/example/hello"
  :depends-on ("clgfw")
  :license "Apache-2.0"
  :description "Basic example"
  :serial t
  :components ((:module "example" 
                :components ((:file "hello"))))
  :build-operation program-op
  :build-pathname "hello"                 ;; shell name
  :entry-point "clgfw/example/hello:main" ;; thunk
  )
