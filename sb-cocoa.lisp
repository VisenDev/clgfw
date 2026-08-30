(defpackage #:clgfw/cocoa
  (:use #:cl #:sb-alien))
(in-package #:clgfw/cocoa)

;; /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/objc/


(load-shared-object "/usr/lib/libSystem.B.dylib")
(load-shared-object
 "/System/Library/Frameworks/Foundation.framework/Foundation")
(load-shared-object "/System/Library/Frameworks/AppKit.framework/AppKit")

(define-alien-type SEL system-area-pointer)
(define-alien-type Class system-area-pointer)
(define-alien-type id system-area-pointer)
(define-alien-type Method system-area-pointer)
(define-alien-type Ivar system-area-pointer)

(defun selector-get (selector-name)
  (alien-funcall
   (extern-alien "sel_registerName"
                          (function SEL (c-string)))
   (make-alien-string selector-name)))

(defun class-get (class-name)
  (alien-funcall (extern-alien "objc_getClass" (function Class (c-string)))
                 (make-alien-string class-name)))

(defun message-send (id selector &rest args)
;; OBJC_EXPORT id _Nullable
;; objc_msgSend(id _Nullable self, SEL _Nonnull op, ...)
;;    OBJC_AVAILABLE(10.0, 2.0, 9.0, 1.0, 2.0);
  )
