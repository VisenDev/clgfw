(defpackage #:clgfw
  (:use :cl)
  (:export #:init-window #:close-window #:*state*))
(in-package #:clgfw)

(defclass state ()
  (
   ;;; ==WAYLAND==
   wl-display ;; Globals
   wl-registry
   wl-shm
   wl-compositor
   xdg-wm-base
   wl-surface ;; Objects
   xdg-surface
   xdg-toplevel

   ;;; ==STATE==
   quit
   (offset :initform 0)
   last-frame
   ))

(defvar *state* (make-instance 'state))

;#+linux
;(defun handle-registry (&rest event)
;  (with-slots (wl-registry wl-shm wl-compositor xdg-wm-base) *state*
;;    (format t "Event: ~a     " event)
;    (destructuring-bind (global interface-id name version) event
;      (declare (ignore global version))
;      (when (string= "wl_compositor" name)
;        (format t "found compositor~%")
;        (setf wl-compositor (wayflan-client:wl-registry.bind
;                             wl-registry interface-id 'wayflan-client:wl-compositor 4)))
;      (when (string= "wl_shm" name)
;        (format t "found shm~%")
;        (setf wl-shm (wayflan-client:wl-registry.bind
;                      wl-registry interface-id 'wayflan-client:wl-shm 1)))
;      (when (string= "xdg_wm_base" name)
;        (format t "found xdg~%")
;        (setf xdg-wm-base (wayflan-client:wl-registry.bind
;                           wl-registry interface-id 'wayflan-client.xdg-shell:xdg-wm-base 1))
;        (push (wayflan-client:evelambda
;                (:ping (serial)
;                       (wayflan-client.xdg-shell:xdg-wm-base.pong xdg-wm-base serial)))
;              (wayflan-client:wl-proxy-hooks xdg-wm-base)))
;
;      )
;    ))

#+linux
(defun handle-registry (&rest event)
  "Registry handler. `registry' is provided by the hook; `event' is the wayland event list.
   This version uses event-ecase to match the :global event payload."
;  (declare (ignore registry))
  (format t "registry event: ~S~%" event)          ; <--- helpful debug line
  (wayflan-client:event-case event
    (:global (name interface version)
     (declare (ignore version))
     (with-slots (wl-registry wl-shm wl-compositor xdg-wm-base) *state*
       ;; debug
       (format t "registry global: name=~S interface=~S~%" name interface)

       (cond
         ((string= interface "wl_compositor")
          (format t "found compositor~%")
          (setf wl-compositor
                (wayflan-client:wl-registry.bind
                  wl-registry name 'wayflan-client:wl-compositor 4)))

         ((string= interface "wl_shm")
          (format t "found shm~%")
          (setf wl-shm
                (wayflan-client:wl-registry.bind
                  wl-registry name 'wayflan-client:wl-shm 1)))

         ((string= interface "xdg_wm_base")
          (format t "found xdg~%")
          (setf xdg-wm-base
                (wayflan-client:wl-registry.bind
                  wl-registry name 'wayflan-client.xdg-shell:xdg-wm-base 1))
          (push (wayflan-client:evelambda
                  (:ping (serial)
                     (wayflan-client.xdg-shell:xdg-wm-base.pong xdg-wm-base serial)))
                (wayflan-client:wl-proxy-hooks xdg-wm-base))))))))


#+linux
(defun draw-frame ()
  (with-slots (wl-shm offset) *state*
    (let* ((width 640)
           (height 480)
           (stride (* width 4))
           (size (* stride height))
           (offset* (floor (mod offset 8)))
           buffer)
      (posix-shm:with-open-shm-and-mmap* (shm pool-data (:direction :io) (size))
        (wayflan-client:with-proxy (pool (wayflan-client:wl-shm.create-pool wl-shm (posix-shm:shm-fd shm) size))
          (setf buffer (wayflan-client:wl-shm-pool.create-buffer
                         pool 0 width height stride
                         :xrgb8888)))

        ;; Draw checkerboxed background
        (dotimes (y height)
          (dotimes (x width)
            (setf (cffi:mem-aref pool-data :uint32 (+ (* y width) x))
                  (if (< (mod (+ (+ x offset*)
                                 (* (floor (+ y offset*) 8) 8)) 16)
                         8)
                      #xff666666
                      #xffeeeeee))))

        (push (wayflan-client:evelambda
                (:release ()
                 ;; Sent by the compositor when it's no longer using this buffer.
                 (wayflan-client:destroy-proxy buffer)))
              (wayflan-client:wl-proxy-hooks buffer))
        buffer))))


#+linux
(defun handle-frame-callback (callback &rest event)
  (format t "Frame callback called!~%")
  (wayflan-client:event-ecase event
    (:done (time)
     (with-slots (last-frame offset wl-surface) *state*
       ;; Destroy this callback
       (wayflan-client:destroy-proxy callback)

       ;; Request another frame
       (setf callback (wayflan-client:wl-surface.frame wl-surface))
       (push (alexandria:curry 'handle-frame-callback callback)
             (wayflan-client:wl-proxy-hooks callback))

      ;; Update scroll amount at 24px/second
      (unless (zerop last-frame)
        (incf offset (* (/ (- time last-frame) 1000.0) 24)))

      ;; Submit a new frame for this event
      (let ((wl-buffer (draw-frame)))
        (wayflan-client:wl-surface.attach wl-surface wl-buffer 0 0)
        (wayflan-client:wl-surface.damage-buffer
          wl-surface 0 0 wayflan-client:+most-positive-wl-int+ wayflan-client:+most-positive-wl-int+)
        (wayflan-client:wl-surface.commit wl-surface))

      (setf last-frame time)))))



(defun init-window (width height title)
  (declare (ignore width height))
  #+linux (print "linux")
  #+darwin (print "darwin")
  #+windows (print "windows")
  #+linux
  (progn
    (with-slots (wl-display wl-registry wl-surface xdg-surface xdg-toplevel wl-compositor xdg-wm-base quit) *state* 
      (setf wl-display (wayflan-client:wl-display-connect))
      (setf wl-registry (wayflan-client:wl-display.get-registry wl-display))
      (setf quit nil)
      (push #'handle-registry
            (wayflan-client:wl-proxy-hooks wl-registry))
      (wayflan-client:wl-display-roundtrip wl-display)
      (setf wl-surface (wayflan-client:wl-compositor.create-surface wl-compositor)
            xdg-surface (wayflan-client.xdg-shell:xdg-wm-base.get-xdg-surface
                         xdg-wm-base wl-surface)
            xdg-toplevel (wayflan-client.xdg-shell:xdg-surface.get-toplevel xdg-surface))
      (push (wayflan-client:evlambda
              (:close () (setf quit t))
              )
            (wayflan-client:wl-proxy-hooks xdg-toplevel))
      (wayflan-client.xdg-shell:xdg-toplevel.set-title xdg-toplevel title)
      (wayflan-client:wl-surface.commit wl-surface)
      
      (let ((cb (wayflan-client:wl-surface.frame wl-surface)))
        (push (alexandria:curry 'handle-frame-callback cb)
              (wayflan-client:wl-proxy-hooks cb)))
      (loop :while (not quit) :do (wayflan-client:wl-display-dispatch-event wl-display))
      )))

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
