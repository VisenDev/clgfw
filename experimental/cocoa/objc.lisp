;;;; THIS FILE CONTAINS EXPERIMENTS IN BUILDING A PORTABLE OBJC
;;;; INTERFACE FOR SBCL USING CFFI



(require "cffi")
(require "cffi-libffi")
(defpackage #:clgfw/objc
  (:use #:cl #:cffi)
  ;; (:export

  ;;  ;; Types
  ;;  #:class_t
  ;;  #:sel_t
  ;;  #:imp_t
  ;;  #:id_t

  ;;  ;; Functions
  ;;  #:class-add-method
  ;;  #:get-class
  ;;  #:sel-register-name)
  )
(in-package #:clgfw/objc)
nn
;;; The definitions for the data structures and typse can be found here (on my system at least)
;;; /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/objc/

(define-foreign-library foundation
    (:darwin (:framework "Foundation")))
(define-foreign-library appkit
  (:darwin (:framework "AppKit")))

(use-foreign-library foundation)
(use-foreign-library appkit)

;; I've chosen to write the objc types the same as they are written in C to make it easier
;; to understand what lisp symbol corresponds to a given C type
(defctype class_t :pointer "Class: An opaque type that represents and objective-c class")
(defctype SEL   :pointer "SEL: An opaque type that represents a method selector.")
(defctype IMP   :pointer "IMP: typedef id _Nullable (*IMP)(id _Nonnull, SEL _Nonnull, ...); ")
(defctype ID    :pointer "id: A pointer to an instance of a class")
(defctype IVar  :pointer "Ivar: An opaque type that represents an instance variable.")
(defctype NSInteger :long)
(defctype NSFloat :double)

(cffi:defcstruct (CGPoint :class CGPoint)
  (x NSFloat)
  (y NSFloat))
;; (defmethod translate-into-foreign-memory ((obj list) (type CGPoint) ptr)
;;   (with-foreign-slots ((x y) ptr (:struct CGPoint))
;;     (setf x (getf obj :x))
;;     (setf y (getf obj :y))))
;; (defmethod translate-from-foreign (ptr (type CGPoint))
;;   (with-foreign-slots ((x y) ptr (:struct CGPoint))
;;     (list :x x :y y))
;;   )

(cffi:defcstruct (CGSize :class CGSize)
  (width NSFloat)
  (height NSFloat))
;; (defmethod translate-into-foreign-memory ((obj list) (type CGSize) ptr)
;;   (with-foreign-slots ((width height) ptr (:struct CGSize))
;;     (setf width (getf obj :width))
;;     (setf height (getf obj :height))))
;; (defmethod translate-from-foreign (ptr (type CGSize))
;;   (with-foreign-slots ((width height) ptr (:struct CGSize))
;;     (list :width width :height height))
;;   )

(cffi:defcstruct (CGRect :class CGRect)
  (origin (:struct CGPoint))
  (size (:struct CGSize)))

;; (defmethod translate-into-foreign-memory ((obj list) (type CGRect) ptr)
;;   (translate-into-foreign-memory
;;    (getf obj :origin)
;;    'CGPoint
;;    (foreign-slot-pointer ptr 'CGRect 'origin))

;;   (translate-into-foreign-memory
;;    (getf obj :size)
;;    'CGSize
;;    (foreign-slot-pointer ptr 'CGRect 'size)))

;; (defmethod translate-from-foreign (ptr (type CGRect))
;;   (list
;;    :origin (convert-from-foreign
;;             (foreign-slot-pointer ptr 'CGRect 'origin)
;;             'CGPoint)
;;    :size   (convert-from-foreign
;;             (foreign-slot-pointer ptr 'CGRect 'size)
;;             'CGSize)))



;; (defmethod translate-into-foreign-memory ((obj list) (type CGRect) ptr)
;;   ;; origin
;;   (translate-into-foreign-memory
;;    (getf obj :origin)
;;    'CGPoint
;;    (foreign-slot-pointer ptr '(:struct CGRect) 'origin))

;;   ;; size
;;   (translate-into-foreign-memory
;;    (getf obj :size)
;;    'CGSize
;;    (foreign-slot-pointer ptr '(:struct CGRect) 'size)))

;; (defmethod translate-from-foreign (ptr (type CGRect))
;;   (list
;;    :origin (convert-from-foreign
;;             (foreign-slot-pointer ptr '(:struct CGRect) 'origin)
;;             '(:struct CGPoint))
;;    :size   (convert-from-foreign
;;             (foreign-slot-pointer ptr '(:struct CGRect) 'size)
;;             '(:struct CGSize))))



;; (defmethod translate-into-foreign-memory ((obj list) (type CGRect) ptr)
;;   (with-foreign-slots ((origin size) ptr (:struct CGRect))
;;     (setf origin (getf obj :origin))
;;     (setf size (getf obj :size))))
;; (defmethod translate-from-foreign (ptr (type CGRect))
;;   (with-foreign-slots ((origin size) ptr (:struct CGRect))
;;     (list :origin origin :size size))
;;   )



(defcfun ("class_addMethod" class-add-method) :bool        
  (class class_t)
  (name SEL)                
  (implementation IMP)
  (types :string))

(defcfun ("objc_getClass" get-class) class_t
  (name :string))

(defcfun ("sel_registerName" @selector) SEL
  (str :string))

(defcfun ("object_getInstanceVariable" get-instance-variable) IVar
  (obj ID)
  (name :string)
  (out-value :pointer) ; void ** outValue
  )


(define-symbol-macro NSObject (get-class "NSObject"))
(define-symbol-macro NSApplication (get-class "NSApplication"))
(define-symbol-macro NSWindow (get-class "NSWindow"))

(defcfun ("objc_msgSend" msg-send) ID
  (id ID) (sel SEL) &rest
  )



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

p;; (defun alloc (class-name)
;;   (msg-send ID (get-class class-name) "alloc"))

(defcfun ("objc_msgSend" objc-msg-send-[id-sel-cgrect-nsint-nsint-bool]->id) ID
  (id ID) (sel SEL) (rect (:struct CGRect)) (style-mask NSInteger) (backing-store-type NSInteger) (defer :boolean)
  )

;;;; TEST CODE
(defun main ()
  
  (let* ((app (msg-send NSApplication (@selector "sharedApplication")))
         (window (msg-send NSWindow (@selector "alloc")))
         (init-selector      (@selector "initWithContentRect:styleMask:backing:defer:"))
         (style-mask (logior +nswindowstylemasktitled+ +nswindowstylemaskresizable+ +nswindowstylemaskclosable+))
         (backing-store 2)
         (rect '(origin (x 1d0 y 2d0)
                 size (width 400d0 height 200d0))))
    (print app)
    (print window)
    (print init-selector)
    (print style-mask)
    (print rect)
    (objc-msg-send-[id-sel-cgrect-nsint-nsint-bool]->id
     window
     init-selector
     rect
     style-mask
     backing-store
     nil
     )
    
    ;; (foreign-funcall "objc_msgSend" ID window
    ;;                  SEL (@selector "initWithContentRect:styleMask:backing:defer:")
    ;;                  (:struct CGRect) rect
    ;;                  NSInteger (logior +nswindowstylemasktitled+ +nswindowstylemaskresizable+ +nswindowstylemaskclosable+)
    ;;                  NSInteger 2
    ;;                  :boolean nil
    ;;                  :long
    ;;                  )
    ;; (msg-send window (@selector "setTitle:") :string "Hello World")
    ;; (msg-send window (@selector "makeKeyAndOrderFront:") :boolean nil)
    ;; (msg-send app (@selector "run"))
    ))










;;;; TEST CODE
;; (defcallback on-close :uint ((self :pointer))
;;   (with-foreign-object (win :pointer)
;;     (object-get-instance-variable self "NSWindow" (cffi:get-var-pointer win))
;;     (format t "Win: ~a~%" win)
;;     )
;;   1
;;   )

;; (defun main ()
;;   (class-add-method (objc-get-class "NSObject")
;;                     (sel-register-name "windowShouldClose:")
;;                     (callback on-close)
;;                     (null-pointer))
;;   (let* ((nsapp (objc-msg-send-id
;;                  (objc-get-class "NSApplication")
;;                  (sel-register-name "sharedApplication" )))
;;          (args (logior +NSWindowStyleMaskClosable+
;;                        +NSWindowStyleMaskMiniaturizable+
;;                        #|+NSBackingStoreBuffered+|# 2
;;                        +NSWindowStyleMaskTitled+
;;                        +NSWindowStyleMaskResizable+)))
;;     (objc-msg-send-void-int nsapp (sel-register-name "setActivationPolicy:") 0)

    
;;     (cffi:with-foreign-objects ((origin '(:struct cg_point_t))
;;                                 (size '(:struct cg_size_t))
;;                                 (rect '(:struct cg_rect_t))) 
;;       (setf (cffi:foreign-slot-value origin '(:struct cg_point_t) 'x) 100d0)
;;       (setf (cffi:foreign-slot-value origin '(:struct cg_point_t) 'y) 100d0)
;;       (setf (cffi:foreign-slot-value size '(:struct cg_size_t) 'width) 100d0)
;;       (setf (cffi:foreign-slot-value size '(:struct cg_size_t) 'height) 100d0)
;;       (setf (cffi:foreign-slot-value rect '(:struct cg_rect_t) 'origin) origin)
;;       (setf (cffi:foreign-slot-value rect '(:struct cg_rect_t) 'size) size)
;;       (foreign-funcall "objc_msgSend"
;;                        id_t (ns-alloc (objc-get-class "NSWindow"))
;;                        sel_t (sel-register-name "initWithContentRect:styleMask:backing:defer:")
;;                        (:struct cg_rect_t) rect
;;                        ns_integer_t args
;;                        ns_integer_t 0
;;                        :bool nil

;;                        ;;return value
;;                        idn_t
;;                        ))
;;                                         ;(id (*)(id, SEL, NSRect, NSWindowStyleMask, NSBackingStoreType, bool))
;;     (format t "nsapp: ~a~%" nsapp))
;;   )
