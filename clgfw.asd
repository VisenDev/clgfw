;;;; Copyright 2026 Robert Wess Burnett
;;;; 
;;;; Licensed under the Apache License, Version 2.0 (the "License");
;;;; you may not use this file except in compliance with the License.
;;;; You may obtain a copy of the License at
;;;; 
;;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;; 
;;;; Unless required by applicable law or agreed to in writing, software
;;;; distributed under the License is distributed on an "AS IS" BASIS,
;;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;;; See the License for the specific language governing permissions and
;;;; limitations under the License.


(in-package #:asdf-user)

(defsystem "clgfw/core"
  :version "0.0.1"
  :author "Robert Wess Burnett"
  :license "Apache-2"
  :description "Common Lisp General Framework for Windowing"
  :depends-on ("local-time" "uiop" "alexandria")
  :serial t
  :components ((:module "src"
                 :components ((:file "package")
                              (:file "common")
                              (:file "colors")))))

(defsystem "clgfw/backend/wayland"
  :depends-on ("clgfw/core"
               "alexandria"
               "wayflan"
               "posix-shm"
               "input-event-codes"
               "cl-xkb")
  :serial t
  :components ((:module "src"
                :components ((:module "bdf"
                              :components ((:file "bdf")
                                           (:file "font-loader")
                                           (:file "renderer")))
                             (:file "backend-wayland")))))

(defsystem "clgfw/backend/x11"
  :depends-on ("clgfw/core" "clx")
  :components ((:module "src"
                 :components ((:file "backend-x11")))))

(defsystem "clgfw/backend/jvm"
  :depends-on ("clgfw/core")
  :components ((:module "src"
                 :components ((:file "backend-jvm")))))

(defsystem "clgfw/backend/curses"
  :depends-on ("clgfw/core" "cl-charms" "alexandria")
  :components ((:module "src"
                 :components ((:file "backend-curses")))))

;; TODO: CLOG backend??
;; TODO: JSCL backend??
;; TODO: win32 backending using ftw

(defsystem "clgfw"
    :version "0.0.1"
    :author "Robert Wess Burnett"
    :license "Apache-2"
    :description "Common Lisp General Framework for Windowing + Platform Appropriate Backends"
    :depends-on ("clgfw/core"
                 (:feature :abcl "clgfw/backend/jvm")
                 (:feature (:or :bsd :linux :unix :macos :macosx :darwin) "clgfw/backend/curses")
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
