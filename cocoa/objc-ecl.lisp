(in-package #:clgfw)

#+ecl
(progn
  ;; (si:load-foreign-module #p"/System/Library/Frameworks/Foundation.framework")
  (cffi:define-foreign-library foundation
      (:darwin (:framework "Foundation")))
  (cffi:define-foreign-library appkit
      (:darwin (:framework "AppKit")))

  (cffi:use-foreign-library foundation)
  (cffi:use-foreign-library appkit)

  (ffi:clines "#define SILICON_IMPLEMENTATION")
  (step (ffi:clines
         #.(format nil "#include \"~a\"" (asdf:system-relative-pathname "clgfw" #p"silicon.h"))
         ))
  )

(defun main ()
  (print "hello"))
