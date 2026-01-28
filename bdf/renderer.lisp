(in-package #:clgfw/bdf)

(defun draw-character (ctx bdf x y color character)
  "Draws a character and returns how much many pixels were used"
  (declare (optimize (speed 3) (safety 0)))

;;;; TODO: this function should rasterize the character
;;;; and cache as an image in the given ctx, rather than
;;;; drawing every bit individually every time
  (let* ((ch (gethash character (chars bdf)))
         (bitmap (bitmap ch)))
    (loop :with space-used = (coerce (width (bbx ch)) 'fixnum)
          :for row :of-type simple-bit-vector :across bitmap
          :for dy :of-type fixnum :from y :by 2
          :do
             (loop :for bit :of-type bit :across row
                   :for dx :of-type fixnum :from x :by 2
                   :when (= 1 bit)
                     :do (clgfw:draw-rectangle ctx dx dy 2 2 color))
          :finally (return (coerce (ash space-used 1) 'fixnum)))))

(defun draw-string (ctx bdf x y color characters)
  (loop :for ch :across characters
        :for delta = (draw-character ctx bdf x y color ch)
        :do (incf x delta)))





(defmethod clgfw:draw-text (ctx x y text-height color str)
  (declare (ignore text-height))
  ;;TODO find which font is closest to text-height
  (draw-string ctx (first *fonts*) x y color str))
