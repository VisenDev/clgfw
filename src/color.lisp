(in-package #:clgfw)

;;; ==== COLORS ====
(deftype color () '(integer 0 #xffffffff))
(deftype u8 () `(and fixnum (integer 0 255)))
(deftype normalized-float () `(single-float 0f0 1f0))

(defmacro define-color-byte-accessor (name offset)
  `(progn
     (declaim (ftype (function (color) fixnum) ,name))
     (defun ,name (color)
       (declare (type color color)
                (optimize (speed 3) (safety 3) (debug 3)))
       (the fixnum (ldb (byte 8 ,offset) color)))
     (define-setf-expander ,name (color &environment env)
       (get-setf-expansion `(ldb (byte 8 ,,offset) ,color) env))))

(define-color-byte-accessor color-r 24)
(define-color-byte-accessor color-g 16)
(define-color-byte-accessor color-b 8)
(define-color-byte-accessor color-a 0)

(declaim (ftype (function (&optional fixnum fixnum fixnum fixnum) color) make-color))
(defun make-color (&optional (r 0) (g 0) (b 0) (a #xff))
  (declare (optimize (speed 3) (safety 1)))
  (let* ((result (the color r))
         (result (the color (ash result 8)))
         (result (the color (logior result g)))
         (result (the color (ash result 8)))
         (result (the color (logior result b)))
         (result (the color (ash result 8)))
         (result (the color (logior result a))))
    (the color result)))

(defun print-color (color &optional (stream *standard-output*))
  (format stream "(clgfw/color:make-color ~a ~a ~a ~a)"
          (color-r color)
          (color-g color)
          (color-b color)
          (color-a color)))

;;; UTILS
(declaim (ftype (function (fixnum) u8) clamp-u8))
(defun clamp-u8 (int)
  (declare (optimize (speed 3)))
  (cond
    ((< int 0) 0)
    ((> int 255) 255)
    (t int)))

(declaim (ftype (function (single-float) normalized-float) clamp-normalized-float))
(defun clamp-normalized-float (float)
  (cond
    ((< float 0f0) 0f0)
    ((> float 1.0f0) 1.0f0)
    (t float)))

(declaim (ftype (function (color) boolean) color-invisible-p))
(defun color-invisible-p (color)
  (declare (optimize (speed 3)))
  (= 0 (color-a color)))

(declaim (ftype (function (color) boolean) color-opaque-p))
(defun color-opaque-p (color)
  (declare (optimize (speed 3)))
  (= 255 (color-a color)))

;;; XRGB
(deftype xrgb () `(unsigned-byte 32))

(declaim (ftype (function (color) xrgb) color->xrgb))
(defun color->xrgb (color)
  (declare (optimize (speed 3)))
  (ash color -8))

(declaim (ftype (function (xrgb) color) xrgb->color))
(defun xrgb->color (xrgb)
  (declare (optimize (speed 3)))
  (+ (ash xrgb 8)
     #xff))

;;; NORMALIZED COLOR
(defstruct (normalized-color (:conc-name norm-color-))
  (r 0f0 :type normalized-float)
  (g 0f0 :type normalized-float)
  (b 0f0 :type normalized-float)
  (a 1.0f0 :type normalized-float))

(declaim (ftype (function (u8) normalized-float) u8->normalized-float))
(defun u8->normalized-float (u8)
  (declare (optimize (speed 3)))
  (/ (coerce u8 'single-float) 255f0))

(declaim (ftype (function (normalized-float) u8) normalized-float->u8))
(defun normalized-float->u8 (normalized-float)
  (declare (optimize (speed 3)))
  (coerce
   (floor (* normalized-float 255f0))
   'u8))

(declaim (ftype (function (color) normalized-color) color->normalized-color))
(defun color->normalized-color (color)
  (make-normalized-color
   :r (u8->normalized-float (color-r color))
   :g (u8->normalized-float (color-g color))
   :b (u8->normalized-float (color-b color))
   :a (u8->normalized-float (color-a color))))

(declaim (ftype (function (normalized-color) color) normalized-color->color))
(defun normalized-color->color (norm-color)
  (make-color
   (normalized-float->u8 (norm-color-r norm-color))
   (normalized-float->u8 (norm-color-g norm-color))
   (normalized-float->u8 (norm-color-b norm-color))
   (normalized-float->u8 (norm-color-a norm-color))))


;;; Luminance
(deftype luminance () `normalized-float)

(defconstant +luminance-red+   0.2126f0)
(defconstant +luminance-green+ 0.7152f0)
(defconstant +luminance-blue+  0.0722f0)

(declaim (ftype (function (normalized-color) luminance) normalized-color->luminance))
(defun normalized-color->luminance (norm-color)
  (+ (* +luminance-red+   (norm-color-r norm-color))
     (* +luminance-green+ (norm-color-g norm-color))
     (* +luminance-blue+  (norm-color-b norm-color))))

(declaim (ftype (function (color) luminance) color->luminance))
(defun color->luminance (color)
  (let ((norm-color (color->normalized-color color)))
    (declare (dynamic-extent norm-color))
    (normalized-color->luminance norm-color)))

(declaim (ftype (function (luminance) normalized-color) luminance->normalized-color))
(defun luminance->normalized-color (luminance)
  (make-normalized-color
   :r luminance
   :g luminance
   :b luminance
   :a 1.0f0))

(declaim (ftype (function (luminance) color) luminance->color))
(defun luminance->color (luminance)
  (normalized-color->color (luminance->normalized-color luminance)))


;;; ALGORITHMS
(declaim (ftype (function (color color) color) color-tint))
(defun color-tint (color tint)
  (let ((luminance (color->luminance color))
        (norm-color (color->normalized-color color))
        (norm-tint (color->normalized-color tint)))
    (normalized-color->color
     (make-normalized-color
      :r (* (norm-color-r norm-tint) luminance)
      :g (* (norm-color-g norm-tint) luminance)
      :b (* (norm-color-b norm-tint) luminance)
      :a (norm-color-a norm-color)))))

(declaim (ftype (function (color) color) color-premultiply-alpha))
(defun color-premultiply-alpha (color)
  (declare (optimize (speed 3)))
  (let* ((norm (color->normalized-color color))
         (a (norm-color-a norm)))
    (declare (dynamic-extent norm))
    (normalized-color->color
     (make-normalized-color
      :r (* a (norm-color-r norm))
      :g (* a (norm-color-g norm))
      :b (* a (norm-color-r norm))
      :a a))))

(declaim (ftype (function (color color) color) color-blend))
(defun color-blend (bg fg)
  (declare (optimize (speed 3)))
  (let* ((nbg (color->normalized-color bg))
         (nfg (color->normalized-color fg))
         
         (bg-r (norm-color-r nbg))
         (bg-g (norm-color-r nbg))
         (bg-b (norm-color-r nbg))
         (bg-a (norm-color-r nbg))
         
         (fg-r (norm-color-r nfg))
         (fg-g (norm-color-g nfg))
         (fg-b (norm-color-b nfg))
         (fg-a (norm-color-a nfg))

         (result-a
           (- 1.0f0
              (* (- 1.0f0 fg-a)
                 (- 1.0f0 bg-a)))))
    (declare (dynamic-extent nfg nbg))
    (normalized-color->color
     (make-normalized-color
      :r (+ (/ (* fg-r fg-a) result-a)
            (/ (* bg-r bg-a (- 1.0f0 fg-a)) result-a))
      :g (+ (/ (* fg-g fg-a) result-a)
            (/ (* bg-g bg-a (- 1.0f0 fg-a)) result-a))
      :b (+ (/ (* fg-b fg-a) result-a)
            (/ (* bg-b bg-a (- 1.0f0 fg-a)) result-a))
      :a result-a))))
