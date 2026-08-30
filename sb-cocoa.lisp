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
(define-alien-type NSPoint
  (struct NSPoint
    (x double)
    (y double)))
(define-alien-type NSSize
  (struct NSSize
    (width double)
    (height double)))
(define-alien-type NSRect
  (struct NSRect
    (origin (struct NSPoint))
    (size   (struct NSSize))))

(defun selector-get (selector-name)
  (alien-funcall
   (extern-alien "sel_registerName"
                 (function SEL c-string))
   (make-alien-string selector-name)))

(defun class-get (class-name)
  (alien-funcall (extern-alien "objc_getClass" (function Class c-string))
                 (make-alien-string class-name)))


(defmacro message-send (return-type id selector &rest arg-type-pairs)
  `(alien-funcall (extern-alien
                   "objc_msgSend"
                   (function ,return-type
                             id SEL ,@(mapcar
                                       #'second
                                       arg-type-pairs)))
                  ,id ,selector ,@(mapcar #'first arg-type-pairs)))

(defun main ()
  (unless (sb-thread:main-thread-p)
    (error "Cocoa requires running from the main thread"))
  
  (sb-int:with-float-traps-masked (:invalid :divide-by-zero)

    (let* ((nsapp-class (class-get "NSApplication"))
           (app (message-send
                 id
                 nsapp-class
                 (selector-get "sharedApplication")))
           
           (nswindow-class (class-get "NSWindow"))
           (window (message-send id nswindow-class (selector-get "alloc")))
           (style 15))

      (message-send boolean app
                    (selector-get "setActivationPolicy:")
                    (0 long))
      (with-alien ((rect (struct NSRect)))
        (setf (slot (slot rect 'origin) 'x) 100.0d0
              (slot (slot rect 'origin) 'y) 100.0d0
              (slot (slot rect 'size) 'width) 800.0d0
              (slot (slot rect 'size) 'height) 600.0d0)
        
        (setf window
              (message-send
               id
               window
               (selector-get "initWithContentRect:styleMask:backing:defer:")
               (rect (struct NSRect))
               (style unsigned-long)
               (2 unsigned-long)
               (nil boolean))))

      (message-send void window (selector-get "center"))
      (message-send void window (selector-get "makeKeyAndOrderFront:")
                    ((sb-sys:int-sap 0) id))
      (message-send void app (selector-get "activateIgnoringOtherApps:")
                    (t boolean))
      (message-send void app (selector-get "run")))))
