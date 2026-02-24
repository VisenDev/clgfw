;;;; THIS FILE IS RESPONSIBLE FOR BUNDLING
;;;; ALL OF THE FONTS IN THE fonts FOLDER

(in-package #:clgfw/bdf)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *fonts*
    (load-bdf
     (asdf:system-relative-pathname
      "clgfw"
      "src/bdf/fonts/terminus/ter-u32n.bdf")))
  ;; (defparameter *fonts*
  ;;   (loop
  ;;     :with dirs = (uiop:subdirectories (asdf:system-relative-pathname "clgfw" "bdf/fonts/"))
  ;;     :for dir :in dirs
  ;;     :appending
  ;;     (loop
  ;;       :with files = (uiop:directory-files dir "*.bdf")
  ;;       :for file :in files
  ;;       :collect (load-bdf file))
  ;;       :into fonts
  ;;     :finally
  ;;        (return (sort fonts (lambda (a b)
  ;;                              (< (point-size a)
  ;;                                 (point-size b)))))))
  )


