(require "cffi")
(defpackage #:clgfw.objc
  (:use #:cl #:cffi)
  (:export

   ;; Types
   #:class_t
   #:sel_t
   #:imp_t
   #:id_t

   ;; Functions
   #:class-add-method
   #:get-class
   #:sel-register-name))
(in-package #:clgfw.objc)

;;; Most code sourced from here
;;; /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/objc/

(define-foreign-library foundation
    (:darwin (:framework "Foundation")))
(define-foreign-library appkit
  (:darwin (:framework "AppKit")))

(use-foreign-library foundation)
(use-foreign-library appkit)


(defctype class_t :pointer "Class: An opaque type that represents and objective-c class")
(defctype sel_t   :pointer "SEL: An opaque type that represents a method selector.")
(defctype imp_t   :pointer "IMP: typedef id _Nullable (*IMP)(id _Nonnull, SEL _Nonnull, ...); ")
(defctype id_t    :pointer "id: A pointer to an instance of a class")
(defctype ivar_t  :pointer "Ivar: An opaque type that represents an instance variable.")


(defcfun ("class_addMethod" class-add-method) :bool        
  (class class_t)
  (name sel_t)                
  (implementation imp_t)
  (types :string))

(defcfun ("objc_getClass" get-class) class_t
  (name :string))

(defcfun ("sel_registerName" sel-register-name) sel_t
  (str :string))

(defcfun ("object_getInstanceVariable" object-get-instance-variable) ivar_t
  (obj id_t)
  (name :string)
  (out-value :pointer) ; void ** outValue
  )


(defun main ()
  (class-add-method) )
