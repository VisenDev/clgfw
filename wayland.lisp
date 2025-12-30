;;;; ==== CLGFW WAYLAND BACKEND ====
;;;; Copyright 2025, by Robert Burnett
;;;; Part of the CLGFW project

(in-package #:clgfw)

(defclass state ()
  (;; Globals
   wl-display
   wl-registry
   wl-shm
   wl-compositor
   xdg-wm-base

   ;; Objects
   wl-surface
   xdg-surface
   xdg-toplevel

   ;; State
   (offset :initform 0.0)
   (last-frame :initform 0)))

(defun init-window (width height title)
  (let ((state (make-instance 'state)))
    (with-slots (wl-display wl-registry wl-shm wl-compositor
                            xdg-wm-base wl-surface xdg-surface xdg-toplevel)
        state
        (wayflan:with-open-display (display)
          ;; Register all globals
          (setf wl-display display
                wl-registry (wayflan:wl-display.get-registry display))
          (push (a:curry 'handle-registry app wl-registry)
                (wl-proxy-hooks wl-registry))
          (wl-display-roundtrip display)

          ;; Create the surface & give it the toplevel role
          (setf wl-surface (wl-compositor.create-surface wl-compositor)
                xdg-surface (xdg-wm-base.get-xdg-surface
                             xdg-wm-base wl-surface)
                xdg-toplevel (xdg-surface.get-toplevel xdg-surface))
          (push (evlambda
                 (:close ()
                         (return-from run)))
                (wl-proxy-hooks xdg-toplevel))
          (push (evelambda
                 (:configure (serial)
                             (xdg-surface.ack-configure xdg-surface serial)
                             (let ((buffer (draw-frame app)))
                               (wl-surface.attach wl-surface buffer 0 0)
                               (wl-surface.commit wl-surface))))
                (wl-proxy-hooks xdg-surface))
          (xdg-toplevel.set-title xdg-toplevel "Example client")
          (wl-surface.commit wl-surface)

          (let ((cb (wl-surface.frame wl-surface)))
            (push (a:curry 'handle-frame-callback app cb)
                  (wl-proxy-hooks cb)))

          (loop (wl-display-dispatch-event display)))))
  )

  
