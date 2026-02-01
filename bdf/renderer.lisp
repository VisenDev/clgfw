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

(defun resize-character (ctx bdf ch target-text-height)
  (let ((scale (/ target-text-height (point-size bdf)))
        (bbx (bbx ch))
        (bitmap (bitmap ch)))
    (make-instance 'bdf-char
                   :bitmap (warp-nearest-neighbor (bitmap ch)
                                                  (floor (* scale (length (aref bitmap 0))))
                                                  target-text-height)
                   :bbx (make-instance 'bounding-box
                                       :offset-x (* (offset-x bbx) scale)
                                       :offset-y (* (offset-y bbx) scale)
                                       :width (* (width bbx) scale)
                                       :height (* (height bbx) scale))
                   :encoding (encoding ch)
                   :startchar (startchar ch)
                   :dwidth (dwidth ch)
                   :swidth (swidth ch))))

(declaim (ftype (function (t bdf character number) bdf-char) get-sized-character))
(defun get-sized-character (ctx bdf lisp-character target-text-height)
  (with-slots (chars) bdf
    (clgfw:when-it (gethash (make-bdf-char-lookup-key
                             :character lisp-character
                             :point-size (floor target-text-height))
                            chars)
        (return-from get-sized-character it))
    (setf (gethash (make-bdf-char-lookup-key
                    :character lisp-character
                    :point-size (floor target-text-height))
                   chars)
          (resize-character
           ctx
           bdf
           (gethash (make-bdf-char-lookup-key
                     :character lisp-character
                     :point-size (point-size bdf))
                    chars)
           (floor target-text-height)))
    (get-sized-character ctx bdf lisp-character target-text-height)))

(defun draw-character (ctx bdf x y text-height color character)
  "Draws a character and returns how much many pixels were used"
  (declare (optimize (speed 3) (safety 3)))
  (let* ((ch (get-sized-character ctx bdf character text-height))
         )
    (with-slots (bitmap bbx) ch
      (declare (type (simple-array simple-bit-vector) bitmap))
      (with-slots (offset-x offset-y) bbx
        (loop :with space-used = (width (bbx ch))
              :for row :of-type simple-bit-vector :across bitmap
              :for dy :of-type fixnum :from (floor (+ y offset-y))
              :do
                 (loop :for bit :of-type bit :across row
                       :for dx :of-type fixnum :from (floor (+ x offset-x))
                       :when (= 1 bit)
                         :do (clgfw:draw-rectangle ctx dx dy 1 1 color))
              :finally (return space-used))))))

(defun draw-string (ctx bdf x y text-height color characters)

  ;;;; TODO add caching of drawn characters as images 
  (loop :for ch :across characters
        :for delta = (draw-character ctx bdf x y text-height color ch)
        :do (incf x delta)))

(defmethod clgfw:draw-text (ctx x y text-height color str)
  ;;TODO find which font is closest to text-height
  (draw-string ctx (first *fonts*) x y text-height color str))
