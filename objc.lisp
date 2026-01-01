(require "cffi")
(require "cffi-libffi")
(defpackage #:clgfw/objc
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
(in-package #:clgfw/objc)

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
(defctype ns_integer_t :long)
(defctype ns_float_t :double)

(cffi:defcstruct cg_point_t
  (x ns_float_t)
  (y ns_float_t))
(cffi:defcstruct cg_size_t
  (width ns_float_t)
  (height ns_float_t))
(cffi:defcstruct cg_rect_t
  (origin (:struct cg_point_t))
  (size (:struct  cg_size_t)))


(defcfun ("class_addMethod" class-add-method) :bool        
  (class class_t)
  (name sel_t)                
  (implementation imp_t)
  (types :string))

(defcfun ("objc_getClass" objc-get-class) class_t
  (name :string))

(defcfun ("sel_registerName" sel-register-name) sel_t
  (str :string))

(defcfun ("object_getInstanceVariable" object-get-instance-variable) ivar_t
  (obj id_t)
  (name :string)
  (out-value :pointer) ; void ** outValue
  )

(defcstruct N)

(defcfun ("objc_msgSend" objc-msg-send-id) id_t
  (id id_t) (sel sel_t))
(defcfun ("objc_msgSend" objc-msg-send-void-int) :void
  (id id_t) (sel sel_t) (number ns_integer_t))

;(id (*)(id, SEL, NSRect, NSWindowStyleMask, NSBackingStoreType, bool))
;(defcfun ("objc_msgSend" objc-window-msg))

;; (defun send-msg (id sel &rest args)
;;   (cffi:foreign-funcall )
;;   )


(defconstant +NSWindowStyleMaskBorderless+             0)
(defconstant +NSWindowStyleMaskTitled+                 (ash 1 0))
(defconstant +NSWindowStyleMaskClosable+               (ash 1 1))
(defconstant +NSWindowStyleMaskMiniaturizable+         (ash 1 2))
(defconstant +NSWindowStyleMaskResizable+              (ash 1 3))
(defconstant +NSWindowStyleMaskTexturedBackground+     (ash 1 8)) #| deprecated |#
(defconstant +NSWindowStyleMaskUnifiedTitleAndToolbar+ (ash 1 12))
(defconstant +NSWindowStyleMaskFullScreen+             (ash 1 14))
(defconstant +NSWindowStyleMaskFullSizeContentView+    (ash 1 15))
(defconstant +NSWindowStyleMaskUtilityWindow+          (ash 1 4))
(defconstant +NSWindowStyleMaskDocModalWindow+         (ash 1 6))
(defconstant +NSWindowStyleMaskNonactivatingPanel+     (ash 1 7))
(defconstant +NSWindowStyleMaskHUDWindow+              (ash 1 13))

(defun ns-alloc (class-id)
  (objc-msg-send-id class-id (sel-register-name "alloc"))
  )

;;;; TEST CODE
(defcallback on-close :uint ((self :pointer))
  (with-foreign-object (win :pointer)
    (object-get-instance-variable self "NSWindow" (cffi:get-var-pointer win))
    (format t "Win: ~a~%" win)
    )
  1
  )

(defun main ()
  (class-add-method (objc-get-class "NSObject")
                    (sel-register-name "windowShouldClose:")
                    (callback on-close)
                    (null-pointer))
  (let* ((nsapp (objc-msg-send-id
                 (objc-get-class "NSApplication")
                 (sel-register-name "sharedApplication" )))
         (args (logior +NSWindowStyleMaskClosable+
                       +NSWindowStyleMaskMiniaturizable+
                       #|+NSBackingStoreBuffered+|# 2
                       +NSWindowStyleMaskTitled+
                       +NSWindowStyleMaskResizable+)))
    (objc-msg-send-void-int nsapp (sel-register-name "setActivationPolicy:") 0)

    
    (cffi:with-foreign-objects ((origin '(:struct cg_point_t))
                                (size '(:struct cg_size_t))
                                (rect '(:struct cg_rect_t))) 
      (setf (cffi:foreign-slot-value origin '(:struct cg_point_t) 'x) 100d0)
      (setf (cffi:foreign-slot-value origin '(:struct cg_point_t) 'y) 100d0)
      (setf (cffi:foreign-slot-value size '(:struct cg_size_t) 'width) 100d0)
      (setf (cffi:foreign-slot-value size '(:struct cg_size_t) 'height) 100d0)
      (setf (cffi:foreign-slot-value rect '(:struct cg_rect_t) 'origin) origin)
      (setf (cffi:foreign-slot-value rect '(:struct cg_rect_t) 'size) size)
      (foreign-funcall "objc_msgSend"
                       id_t (ns-alloc (objc-get-class "NSWindow"))
                       sel_t (sel-register-name "initWithContentRect:styleMask:backing:defer:")
                       (:struct cg_rect_t) rect
                       ns_integer_t args
                       ns_integer_t 0
                       :bool nil

                       ;;return value
                       idn_t
                       ))
                                        ;(id (*)(id, SEL, NSRect, NSWindowStyleMask, NSBackingStoreType, bool))
    (format t "nsapp: ~a~%" nsapp))
  )
