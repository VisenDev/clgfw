(in-package #:clgfw/bdf)

(defclass character-cache ()
  ((sizes :accessor sizes
          :initform (make-hash-table :test #'=)
          :documentation "a hashmap indexed by text-height containing an image of the
                          character drawn at the specified text-height")))

(defclass font-cache-mixin ()
  ((cached-characters :accessor cached-characters
                      :initform (make-hash-table :test #'char-equal :size 256)))
  )


;;TODO change bitmaps so they are stored in 2 dimensional bitvectors, rather than nested vectors

(defun warp-nearest-neighbor (bitmap width height)
  ;; (declare (type (simple-array simple-bit-vector *) bitmap))
  (let ((result (make-array height :element-type `(simple-bit-vector ,width)
                                   :initial-contents
                                   (loop :repeat height
                                         :collect (make-array width
                                                              :element-type 'bit
                                                              :initial-element 0)))))
    (loop :with src-height = (length bitmap)
          :with src-width = (length (aref bitmap 0))
          :with scale-x = (/ src-width width)
          :with scale-y = (/ src-height height)
          :for row :across result
          :for y :from 0
          :for src-y = (min (1- src-height) (floor (* y scale-y)))
          :do
             (loop :for bit :across row
                   :for x :from 0
                   :for src-x = (min (1- src-width) (floor (* x scale-x)))
                   :for nearest :of-type bit = (bit (aref bitmap src-y) src-x)
                   :do (setf (bit (aref result y) x) nearest)))
    (return-from warp-nearest-neighbor result)
    )
  )

(defun draw-character (ctx bdf x y text-height color character)
  "Draws a character and returns how much many pixels were used"
  (declare (optimize (speed 3) (safety 0)))
  (let* ((ch (gethash character (chars bdf)))
         (scale (/  text-height (point-size bdf)))
         (bitmap (warp-nearest-neighbor (bitmap ch)
                                        (floor (* scale (length (aref (bitmap ch) 0))))
                                        text-height)))
    (declare (type (simple-array simple-bit-vector) bitmap))
    (loop :with space-used = (floor (* scale (coerce (width (bbx ch)) 'fixnum)))
          :for row :of-type simple-bit-vector :across bitmap
          :for dy :of-type fixnum :from y
          :do
             (loop :for bit :of-type bit :across row
                   :for dx :of-type fixnum :from x
                   :when (= 1 bit)
                     :do (clgfw:draw-rectangle ctx dx dy 1 1 color))
          :finally (return space-used))))

(defun draw-string (ctx bdf x y text-height color characters)

  ;;;; TODO add caching of drawn characters as images 
  (loop :for ch :across characters
        :for delta = (draw-character ctx bdf x y text-height color ch)
        :do (incf x delta)))

(defmethod clgfw:draw-text (ctx x y text-height color str)
  ;;TODO find which font is closest to text-height
  (draw-string ctx (last (first *fonts*)) x y text-height color str))
