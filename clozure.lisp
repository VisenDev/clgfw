(in-package #:clgfw)

#-clozure
(error "The clozure macos backend only works with clozure common lisp")

(require "COCOA")



;;;; TESTING CODE
(defun main ()
  (objc:@class "NSWindow")
  )
