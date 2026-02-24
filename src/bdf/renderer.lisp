(in-package #:clgfw/bdf)

;; (defclass character-cache ()
;;   ((sizes :accessor sizes
;;           :initform (make-hash-table :test #'=)
;;           :documentation "a hashmap indexed by text-height containing an image of the
;;                           character drawn at the specified text-height")))

;; (defclass font-cache-mixin ()
;;   ((cached-characters :accessor cached-characters
;;                       :initform (make-hash-table :test #'char-equal :size 256)))
;;   )


;;TODO change bitmaps so they are stored in 2 dimensional bitvectors, rather than nested vectors

(defun warp-nearest-neighbor (bitmap width height)
  ;; (declare (type (simple-array simple-bit-vector *) bitmap))
  (let ((result (make-array (list height width) :element-type 'bit)))
    (loop
      :with src-height = (array-dimension bitmap 0)
      :with src-width = (array-dimension bitmap 1)
      :with scale-x = (/ src-width width)
      :with scale-y = (/ src-height height)
      :for y :from 0 :below height
      :for src-y = (min (1- src-height) (floor (* y scale-y)))
      :do
         (loop
           :for x :from 0 :below width
           :for src-x = (min (1- src-width) (floor (* x scale-x)))
           :for nearest :of-type bit = (aref bitmap src-y src-x)
           :do (setf (aref result y x) nearest)))
    (return-from warp-nearest-neighbor result)))

(defun resize-character (bdf ch target-text-height)
  ch
  ;; (return-from resize-character ch)
  ;; (if (= target-text-height (height bdf))
  ;;     ch
  ;;     (let ((scale (/ target-text-height (point-size bdf)))
  ;;           (bbx (bbx ch))
  ;;           (bitmap (bitmap ch)))
  ;;       (make-instance 'bdf-char

  ;;                      ;; TODO re-implement warp nearest neighbor resizing
  ;;                      :bitmap (warp-nearest-neighbor (bitmap ch)
  ;;                                                     (floor (* scale (array-dimension bitmap 1)))
  ;;                                                     target-text-height)
  ;;                      :bbx (make-instance 'bounding-box
  ;;                                          :offset-x (ceiling (* (offset-x bbx) scale))
  ;;                                          :offset-y (ceiling (* (offset-y bbx) scale))
  ;;                                          :width    (ceiling (* (width bbx) scale)   )
  ;;                                          :height   (ceiling (* (height bbx) scale)) )
  ;;                      :encoding (encoding ch)
  ;;                      :startchar (startchar ch)
  ;;                      :dwidth (dwidth ch)
  ;;                      :swidth (swidth ch))))
  )

(declaim (ftype (function (bdf character number) bdf-char) get-sized-character))
(defun get-sized-character (bdf lisp-character target-text-height)
  (with-slots (chars) bdf
    (when-let (ch (gethash (make-bdf-char-lookup-key
                            :character lisp-character
                            :point-size (floor target-text-height))
                           chars))
        (return-from get-sized-character ch))
    (setf (gethash (make-bdf-char-lookup-key
                    :character lisp-character
                    :point-size (floor target-text-height))
                   chars)
          (resize-character bdf
                            (gethash (make-bdf-char-lookup-key
                                      :character lisp-character
                                      :point-size (point-size bdf))
                                     chars)
                            (floor target-text-height)))
    (get-sized-character bdf lisp-character target-text-height)))

(defun draw-character (backend bdf x y text-height color character)
  "Draws a character and returns how much many pixels were used"
  (declare (optimize (debug 3) (safety 3)))
  (let* ((ch (get-sized-character bdf character text-height)))
    (if-let (canvas (gethash backend (canvases ch)))

      ;; then
      (let ((bbx (bbx ch)))
        (clgfw:backend-draw-canvas
         backend
         (+ x (offset-x bbx))
         (+ y (offset-y bbx))
         canvas
         color)
        (return-from draw-character
          (width bbx)))

      ;; else
      (with-slots (bitmap bbx canvases) ch
        (let ((canvas (clgfw:backend-create-canvas backend 
                                                   ;; (* 2)
                                                   (round (width bbx)) 
                                                   ;; (* 2)
                                                   (round (height bbx))
                                                   )))
          ;; (inspect canvas)
          ;; (inspect bbx)
          ;; (inspect ch)
          (format t "rasterizing character: ~a~%" (startchar ch))
          (loop :with space-used = (width (bbx ch))
                ;; :for row :of-type simple-bit-vector :across bitmap
                :for dy :of-type fixnum :from 0 :below (array-dimension bitmap 0)
                :do
                   (loop :for dx :of-type fixnum :from 0 :below (array-dimension bitmap 1)
                         :when (= 1 (aref bitmap dy dx ))
                           :do (clgfw:backend-draw-rectangle-on-canvas
                                backend canvas
                                dx dy 1 1 clgfw/color:+white+))
                :finally
                   (setf (gethash backend canvases) canvas)
                   (return (draw-character backend bdf x y text-height color character))))))))

(defun draw-string (backend bdf x y text-height color characters)
  (loop :for ch :across characters
        :for delta = (draw-character backend bdf x y text-height color ch)
        :do (incf x delta)))

