(in-package #:clgfw)


#.(use-package '(#:wayflan #:wayflan-client #:wayflan-client.xdg-shell))

;;; This program is based on several examples from the wayflan repo
;;;
;;; This example is a rewrite of the extended example code found in Drew
;;; Devault's The Wayland Protocol, §7.3 thru §8.2. The program creates
;;; a toplevel surface that shows a moving checkerboard grid.
;;;
;;; Copyright (c) 2025 Robert Burnett <>
;;; This work isn't licensed yet
;;; See LICENSE for more details.

(defclass render-buffer () (
   (pool-data :accessor pool-data
              :initarg :pool-data
              :documentation "This is the actual memory buffer that can be aref'd to write pixels")
   (buffer :accessor buffer
           :initarg :buffer
           :documentation "The wayland buffer made from the pool data")
   ))

(defclass ctx/wayland ()
  (;; Globals
   (wl-display :accessor wl-display)
   wl-registry
   wl-shm
   wl-compositor
   xdg-wm-base

   ;; Objects
   wl-surface
   xdg-surface
   xdg-toplevel
   pool
   
   shm
   (backing-pool-data :accessor backing-pool-data)
   (backing-pool-data-size :accessor backing-pool-data-size)
   
   (front-buffer :accessor front-buffer :type render-buffer)
   (back-buffer :accessor back-buffer :type render-buffer)
   
   (need-next-frame :accessor need-next-frame :initform t)
   (width :initform 640 :accessor width)
   (height :initform 480 :accessor height)

   ;; State
   (offset :initform 0.0)
   (last-frame :initform 0)
   (configured :accessor configured :initform nil)
   (window-should-close :initform nil :accessor window-should-close))
  
  (:documentation "An example Wayland application"))

(defmethod begin-drawing ((ctx ctx/wayland))
  )

(defmethod end-drawing ((ctx ctx/wayland))
  (when (window-should-close ctx)
    (return-from end-drawing))
  
  (unless (configured ctx)
    (loop
      (wl-display-dispatch-event (wl-display ctx))
      (when (configured ctx) (return))))

  (loop :until (need-next-frame ctx)
        :do ;; (format t "Waiting for next frame to be needed...~%")
            (sleep 0.001)
            (wl-display-dispatch-event (wl-display ctx)))
  (setf (need-next-frame ctx) nil)
  (with-slots (wl-surface) ctx
    (let ((wl-buffer (buffer (back-buffer ctx))))
      (wl-surface.attach wl-surface wl-buffer 0 0)
      (wl-surface.damage-buffer
       wl-surface 0 0 +most-positive-wl-int+ +most-positive-wl-int+)
      (wl-surface.commit wl-surface)))

  ;;swap front and back buffer
  (let* ((fb (front-buffer ctx))
         (bb (back-buffer ctx)))
    (setf (front-buffer ctx) bb)
    (setf (back-buffer ctx) fb))
  )

(defun color-to-xrbg (color)
  (declare (type color color))
  (let* ((result #xff)
         (result (ash result 8))
         (result (+ result (color-r color)))
         (result (ash result 8))
         (result (+ result (color-g color)))
         (result (ash result 8))
         (result (+ result (color-b color)))
         )
    result))

(defmethod draw-rectangle ((ctx ctx/wayland) x y w h color)
  (let* ((pool-data (pool-data (back-buffer ctx)))
         (stride (* (width ctx) 4))        ; bytes per row
         (row-pixels (/ stride 4))
         (x-end (min (+ x w) (width ctx)))
         (y-end (min (+ y h) (height ctx))))
    (loop :for dy :from y :below y-end :do
      (loop :for dx :from x :below x-end :do
        (setf (cffi:mem-aref pool-data :uint32
                              (+ dx (* dy row-pixels)))
              (color-to-xrbg color))))))


(defun handle-frame-callback (ctx callback &rest event)
  ;; (format t "Handing frame callback ~a~%" callback)
  (event-ecase event
    (:done (time)
           (declare (ignore time))
           (with-slots (last-frame offset wl-surface) ctx
             ;; Destroy this callback
             (destroy-proxy callback)

             ;; Request another frame
             (setf callback (wl-surface.frame wl-surface))
             (push (alexandria:curry 'handle-frame-callback ctx callback)
                   (wl-proxy-hooks callback))

             (setf (need-next-frame ctx) t)
             ))))

(defun handle-registry (app registry &rest event)
  (with-slots (wl-shm wl-compositor xdg-wm-base) app
    (event-case event
      (:global (name interface version)
       (declare (ignore version))
       (case (alexandria:when-let ((it (find-interface-named interface)))
               (class-name it))
         (wl-shm
           (format t "found shm~%")
           (setf wl-shm (wl-registry.bind
                          registry name 'wl-shm 1)))
         (wl-compositor
           (format t "found compositor~%")
           (setf wl-compositor (wl-registry.bind
                                 registry name 'wl-compositor 4)))
         (xdg-wm-base
           (format t "found xdg~%")
           (setf xdg-wm-base (wl-registry.bind
                               registry name 'xdg-wm-base 1))
           (push (evelambda
                   (:ping (serial)
                    (xdg-wm-base.pong xdg-wm-base serial)))
                 (wl-proxy-hooks xdg-wm-base))))))))

(defmethod close-window ((ctx ctx/wayland))
  (with-slots (shm) ctx
    (posix-shm:close-shm shm)
    (posix-shm:munmap (backing-pool-data ctx) (backing-pool-data-size ctx)))

  (wl-display-disconnect (wl-display ctx)))

(defun init-window/wayland (width height title)
  (declare (ignore width height))
  (let ((ctx (make-instance 'ctx/wayland)))
    (with-slots (wl-display wl-registry wl-shm wl-compositor
                 xdg-wm-base wl-surface xdg-surface xdg-toplevel
                 width height shm front-buffer back-buffer pool)
        ctx
        
      ;; Register all globals
      (setf wl-display (wl-display-connect)
            wl-registry (wl-display.get-registry wl-display))
      (push (alexandria:curry 'handle-registry ctx wl-registry)
            (wl-proxy-hooks wl-registry))
      (wl-display-roundtrip wl-display)

      ;; Allocate shm
      (setf shm (posix-shm:open-shm* :direction :io))
      (let* ((stride (* width 4))
             (buffer-size (* height stride))
             (total-size (* buffer-size 2))
             )
        (posix-shm:truncate-shm shm total-size)
        (setf pool (wl-shm.create-pool wl-shm (posix-shm:shm-fd shm) total-size))
        (setf (backing-pool-data ctx) (posix-shm:mmap-shm shm total-size))
        (setf (backing-pool-data-size ctx) total-size)
        (setf front-buffer
              (make-instance 'render-buffer
                             :pool-data (backing-pool-data ctx)
                             :buffer (wl-shm-pool.create-buffer
                                      pool 0 width height stride :xrgb8888)
                             ))
        (setf back-buffer
              (make-instance 'render-buffer
                             :pool-data (cffi:inc-pointer (backing-pool-data ctx) buffer-size)
                             :buffer (wl-shm-pool.create-buffer
                                      pool buffer-size width height stride :xrgb8888)
                             ))
         (push (evelambda
                 (:release ()
                           ))
               (wl-proxy-hooks (buffer front-buffer)))

         (push (evelambda
                 (:release ()
                           ))
               (wl-proxy-hooks (buffer back-buffer)))
        )


      ;; Create the surface & give it the toplevel role
      (setf wl-surface (wl-compositor.create-surface wl-compositor)
            xdg-surface (xdg-wm-base.get-xdg-surface
                         xdg-wm-base wl-surface)
            xdg-toplevel (xdg-surface.get-toplevel xdg-surface))
      (push (evlambda
              (:close ()
                      (setf (window-should-close ctx) t)))
            (wl-proxy-hooks xdg-toplevel))
      (push (evelambda
              (:configure (serial)
                          (format t "configure received serial=~a~%" serial)
                          (xdg-surface.ack-configure xdg-surface serial)
                          (setf (configured ctx) t)
                          ))
            (wl-proxy-hooks xdg-surface))
      (xdg-toplevel.set-title xdg-toplevel title)
      (wl-surface.commit wl-surface)


      (let ((cb (wl-surface.frame wl-surface)))
        (push (alexandria:curry 'handle-frame-callback ctx cb)
              (wl-proxy-hooks cb)))
      (wl-display-roundtrip wl-display)
      (return-from init-window/wayland ctx)
      )
    )
  )

(defun my/run ()
  (let ((app (init-window/wayland 100 100 "foo"))
        (x 10)
        (y 35)
        (dx 1)
        (dy 1))
    (loop :until (window-should-close app) :do
          (begin-drawing app)
          (draw-rectangle app 0 0 300 300 (make-color :r 130 :g 200 :b 220))
          (draw-rectangle app x y 30 30 (make-color :r 10 :g 200 :b 20))
          (when (> x 270)
            (setf dx -1))
          (when (< x 0)
            (setf dx 1))
          (when (> y 270)
            (setf dy -1))
          (when (< y 0)
            (setf dy 1))
          (setf x (+ x dx))
          (setf y (+ y dy))
          (end-drawing app))
    (close-window app)))
