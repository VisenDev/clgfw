(defpackage #:clgfw
  (:use :cl)
  (:export #:init-window #:close-window))
(in-package #:clgfw)

(defclass state ()
  (
   ;;; ==WAYLAND==
   wl-display ;; Globals
   wl-registry
   wl-shm
   wl-compositor
   xdg-wm-base
   ;wl-surface ;; Objects
   ;xdg-surface
   ;xdg-toplevel
   ))

(defvar *state* (make-instance 'state))

(defun handle-registry (&rest event)
  (with-slots (wl-registry wl-shm wl-compositor xdg-wm-base) *state*
    (wayflan-client:event-case event
      (:global (name interface version)
       (declare (ignore version))
       (case (alexandria:when-let ((it (wayflan-client:find-interface-named interface)))
               (class-name it))
         (wl-shm
           (format t "found shm~%")
           (setf wl-shm (wayflan-client:wl-registry.bind
                          wl-registry name 'wl-shm 1)))
         (wl-compositor
           (format t "found compositor~%")
           (setf wl-compositor (wayflan-client:wl-registry.bind
                                 wl-registry name 'wl-compositor 4)))
         (xdg-wm-base
           (format t "found xdg~%")
           (setf xdg-wm-base (wayflan-client:wl-registry.bind
                               wl-registry name 'xdg-wm-base 1))
           (push (wayflan-client:evelambda
                   (:ping (serial)
                    (wayflan-client.xdg-shell:xdg-wm-base.pong xdg-wm-base serial)))
                 (wayflan-client:wl-proxy-hooks xdg-wm-base))))))))


(defun init-window (width height title)
  (declare (ignore width height title))
  #+linux (print "linux")
  #+darwin (print "darwin")
  #+windows (print "windows")
  #+linux
  (progn
    (with-slots (wl-display wl-registry) *state* 
      (setf wl-display (wayflan-client:wl-display-connect))
      (setf wl-registry (wayflan-client:wl-display.get-registry wl-display))
      (push 'handle-registry
            (wayflan-client:wl-proxy-hooks wl-registry))

      )
    ))

(defun close-window ()
  #+linux (print "linux")
  #+darwin (print "darwin")
  #+windows (print "windows")
  #+linux
  (progn
    (with-slots (wl-display) *state* 
      (wayflan-client:wl-display-disconnect wl-display))
    )
  )  
