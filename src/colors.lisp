;;;; COLOR.LISP
;;;;
;;;; DEFINES A BUNCH OF NAMED COLOR CONSTANTS
;;;;
;;;; Copyright 2026 Robert Wess Burnett
;;;; 
;;;; Licensed under the Apache License, Version 2.0 (the "License");
;;;; you may not use this file except in compliance with the License.
;;;; You may obtain a copy of the License at
;;;; 
;;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;; 
;;;; Unless required by applicable law or agreed to in writing, software
;;;; distributed under the License is distributed on an "AS IS" BASIS,
;;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;;; See the License for the specific language governing permissions and
;;;; limitations under the License.

(uiop:define-package #:clgfw/color
  (:use #:cl)
  (:export #:u8
           #:normalized-float

           ;; RGBA
           #:make-color
           #:color
           #:color-r
           #:color-g
           #:color-b
           #:color-a
           #:color-invisible-p
           #:color-opaque-p
           #:color-premultiply-alpha
           #:color-blend
           #:color-tint
           #:color->normalized-color
           #:color->luminance
           #:color->xrgb

           ;; XRGB
           #:xrgb
           #:xrgb->color

           ;; NORMALIZED-COLORS
           #:normalized-color
           #:normalized-color->color
           #:normalized-color->luminance
           #:norm-color-r
           #:norm-color-g
           #:norm-color-b
           #:norm-color-a

           ;; LUMINANCE
           #:luminance
           #:luminance->normalized-color
           #:luminance->color
           #:+luminance-red+
           #:+luminance-green+
           #:+luminance-blue+
           
           ;; Html Color Constants
           #:+IndianRed+ #:+LightCoral+ #:+Salmon+ #:+DarkSalmon+          
           #:+LightSalmon+ #:+Crimson+ #:+Red+ #:+FireBrick+           
           #:+DarkRed+ #:+Pink+ #:+LightPink+ #:+HotPink+             
           #:+DeepPink+ #:+MediumVioletRed+ #:+PaleVioletRed+ #:+LightSalmon+         
           #:+Coral+ #:+Tomato+ #:+OrangeRed+ #:+DarkOrange+          
           #:+Orange+ #:+Gold+ #:+Yellow+ #:+LightYellow+         
           #:+LemonChiffon+ #:+LightGoldenrodYellow+ #:+PapayaWhip+ #:+Moccasin+            
           #:+PeachPuff+ #:+PaleGoldenrod+ #:+Khaki+ #:+DarkKhaki+           
           #:+Lavender+ #:+Thistle+ #:+Plum+ #:+Violet+              
           #:+Orchid+ #:+Fuchsia+ #:+Magenta+ #:+MediumOrchid+        
           #:+MediumPurple+ #:+RebeccaPurple+ #:+BlueViolet+ #:+DarkViolet+          
           #:+DarkOrchid+ #:+DarkMagenta+ #:+Purple+ #:+Indigo+              
           #:+SlateBlue+ #:+DarkSlateBlue+ #:+MediumSlateBlue+ #:+GreenYellow+         
           #:+Chartreuse+ #:+LawnGreen+ #:+Lime+ #:+LimeGreen+           
           #:+PaleGreen+ #:+LightGreen+ #:+MediumSpringGreen+ #:+SpringGreen+         
           #:+MediumSeaGreen+ #:+SeaGreen+ #:+ForestGreen+ #:+Green+               
           #:+DarkGreen+ #:+YellowGreen+ #:+OliveDrab+ #:+Olive+               
           #:+DarkOliveGreen+ #:+MediumAquamarine+ #:+DarkSeaGreen+ #:+LightSeaGreen+       
           #:+DarkCyan+ #:+Teal+ #:+Aqua+ #:+Cyan+                
           #:+LightCyan+ #:+PaleTurquoise+ #:+Aquamarine+ #:+Turquoise+           
           #:+MediumTurquoise+ #:+DarkTurquoise+ #:+CadetBlue+ #:+SteelBlue+           
           #:+LightSteelBlue+ #:+PowderBlue+ #:+LightBlue+ #:+SkyBlue+             
           #:+LightSkyBlue+ #:+DeepSkyBlue+ #:+DodgerBlue+ #:+CornflowerBlue+      
           #:+MediumSlateBlue+ #:+RoyalBlue+ #:+Blue+ #:+MediumBlue+          
           #:+DarkBlue+ #:+Navy+ #:+MidnightBlue+ #:+Cornsilk+            
           #:+BlanchedAlmond+ #:+Bisque+ #:+NavajoWhite+ #:+Wheat+               
           #:+BurlyWood+ #:+Tan+ #:+RosyBrown+ #:+SandyBrown+          
           #:+Goldenrod+ #:+DarkGoldenrod+ #:+Peru+ #:+Chocolate+           
           #:+SaddleBrown+ #:+Sienna+ #:+Brown+ #:+Maroon+              
           #:+White+ #:+Snow+ #:+HoneyDew+ #:+MintCream+           
           #:+Azure+ #:+AliceBlue+ #:+GhostWhite+ #:+WhiteSmoke+          
           #:+SeaShell+ #:+Beige+ #:+OldLace+ #:+FloralWhite+         
           #:+Ivory+ #:+AntiqueWhite+ #:+Linen+ #:+LavenderBlush+       
           #:+MistyRose+ #:+Gainsboro+ #:+LightGray+ #:+Silver+              
           #:+DarkGray+ #:+Gray+ #:+DimGray+ #:+LightSlateGray+      
           #:+SlateGray+ #:+DarkSlateGray+ #:+Black+
           
           ;; My custom colors
           #:+Moon+ #:+Space+))

(in-package #:clgfw/color)

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

;;; COLOR CONSTANTS
(defmacro define-color (name r g b)
  `(defconstant ,name (make-color ,r ,g ,b)))

;;; HTML 5 COLORS
(define-color +IndianRed+             205 92  92)
(define-color +LightCoral+            240 128 128)
(define-color +Salmon+                250 128 114)
(define-color +DarkSalmon+            233 150 122)
(define-color +LightSalmon+           255 160 122)
(define-color +Crimson+               220 20  60)
(define-color +Red+                   255 0   0)
(define-color +FireBrick+             178 34  34)
(define-color +DarkRed+               139 0   0)
(define-color +Pink+                  255 192 203)
(define-color +LightPink+             255 182 193)
(define-color +HotPink+               255 105 180)
(define-color +DeepPink+              255 20  147)
(define-color +MediumVioletRed+       199 21  133)
(define-color +PaleVioletRed+         219 112 147)
(define-color +LightSalmon+           255 160 122)
(define-color +Coral+                 255 127 80)
(define-color +Tomato+                255 99  71)
(define-color +OrangeRed+             255 69  0)
(define-color +DarkOrange+            255 140 0)
(define-color +Orange+                255 165 0)
(define-color +Gold+                  255 215 0)
(define-color +Yellow+                255 255 0)
(define-color +LightYellow+           255 255 224)
(define-color +LemonChiffon+          255 250 205)
(define-color +LightGoldenrodYellow+  250 250 210)
(define-color +PapayaWhip+            255 239 213)
(define-color +Moccasin+              255 228 181)
(define-color +PeachPuff+             255 218 185)
(define-color +PaleGoldenrod+         238 232 170)
(define-color +Khaki+                 240 230 140)
(define-color +DarkKhaki+             189 183 107)
(define-color +Lavender+              230 230 250)
(define-color +Thistle+               216 191 216)
(define-color +Plum+                  221 160 221)
(define-color +Violet+                238 130 238)
(define-color +Orchid+                218 112 214)
(define-color +Fuchsia+               255 0   255)
(define-color +Magenta+               255 0   255)
(define-color +MediumOrchid+          186 85  211)
(define-color +MediumPurple+          147 112 219)
(define-color +RebeccaPurple+         102 51  153)
(define-color +BlueViolet+            138 43  226)
(define-color +DarkViolet+            148 0   211)
(define-color +DarkOrchid+            153 50  204)
(define-color +DarkMagenta+           139 0   139)
(define-color +Purple+                128 0   128)
(define-color +Indigo+                75  0   130)
(define-color +SlateBlue+             106 90  205)
(define-color +DarkSlateBlue+         72  61  139)
(define-color +MediumSlateBlue+       123 104 238)
(define-color +GreenYellow+           173 255 47)
(define-color +Chartreuse+            127 255 0)
(define-color +LawnGreen+             124 252 0)
(define-color +Lime+                  0   255 0)
(define-color +LimeGreen+             50  205 50)
(define-color +PaleGreen+             152 251 152)
(define-color +LightGreen+            144 238 144)
(define-color +MediumSpringGreen+     0   250 154)
(define-color +SpringGreen+           0   255 127)
(define-color +MediumSeaGreen+        60  179 113)
(define-color +SeaGreen+              46  139 87)
(define-color +ForestGreen+           34  139 34)
(define-color +Green+                 0   128 0)
(define-color +DarkGreen+             0   100 0)
(define-color +YellowGreen+           154 205 50)
(define-color +OliveDrab+             107 142 35)
(define-color +Olive+                 128 128 0)
(define-color +DarkOliveGreen+        85  107 47)
(define-color +MediumAquamarine+      102 205 170)
(define-color +DarkSeaGreen+          143 188 139)
(define-color +LightSeaGreen+         32  178 170)
(define-color +DarkCyan+              0   139 139)
(define-color +Teal+                  0   128 128)
(define-color +Aqua+                  0   255 255)
(define-color +Cyan+                  0   255 255)
(define-color +LightCyan+             224 255 255)
(define-color +PaleTurquoise+         175 238 238)
(define-color +Aquamarine+            127 255 212)
(define-color +Turquoise+             64  224 208)
(define-color +MediumTurquoise+       72  209 204)
(define-color +DarkTurquoise+         0   206 209)
(define-color +CadetBlue+             95  158 160)
(define-color +SteelBlue+             70  130 180)
(define-color +LightSteelBlue+        176 196 222)
(define-color +PowderBlue+            176 224 230)
(define-color +LightBlue+             173 216 230)
(define-color +SkyBlue+               135 206 235)
(define-color +LightSkyBlue+          135 206 250)
(define-color +DeepSkyBlue+           0   191 255)
(define-color +DodgerBlue+            30  144 255)
(define-color +CornflowerBlue+        100 149 237)
(define-color +MediumSlateBlue+       123 104 238)
(define-color +RoyalBlue+             65  105 225)
(define-color +Blue+                  0   0   255)
(define-color +MediumBlue+            0   0   205)
(define-color +DarkBlue+              0   0   139)
(define-color +Navy+                  0   0   128)
(define-color +MidnightBlue+          25  25  112)
(define-color +Cornsilk+              255 248 220)
(define-color +BlanchedAlmond+        255 235 205)
(define-color +Bisque+                255 228 196)
(define-color +NavajoWhite+           255 222 173)
(define-color +Wheat+                 245 222 179)
(define-color +BurlyWood+             222 184 135)
(define-color +Tan+                   210 180 140)
(define-color +RosyBrown+             188 143 143)
(define-color +SandyBrown+            244 164 96)
(define-color +Goldenrod+             218 165 32)
(define-color +DarkGoldenrod+         184 134 11)
(define-color +Peru+                  205 133 63)
(define-color +Chocolate+             210 105 30)
(define-color +SaddleBrown+           139 69  19)
(define-color +Sienna+                160 82  45)
(define-color +Brown+                 165 42  42)
(define-color +Maroon+                128 0   0)
(define-color +White+                 255 255 255)
(define-color +Snow+                  255 250 250)
(define-color +HoneyDew+              240 255 240)
(define-color +MintCream+             245 255 250)
(define-color +Azure+                 240 255 255)
(define-color +AliceBlue+             240 248 255)
(define-color +GhostWhite+            248 248 255)
(define-color +WhiteSmoke+            245 245 245)
(define-color +SeaShell+              255 245 238)
(define-color +Beige+                 245 245 220)
(define-color +OldLace+               253 245 230)
(define-color +FloralWhite+           255 250 240)
(define-color +Ivory+                 255 255 240)
(define-color +AntiqueWhite+          250 235 215)
(define-color +Linen+                 250 240 230)
(define-color +LavenderBlush+         255 240 245)
(define-color +MistyRose+             255 228 225)
(define-color +Gainsboro+             220 220 220)
(define-color +LightGray+             211 211 211)
(define-color +Silver+                192 192 192)
(define-color +DarkGray+              169 169 169)
(define-color +Gray+                  128 128 128)
(define-color +DimGray+               105 105 105)
(define-color +LightSlateGray+        119 136 153)
(define-color +SlateGray+             112 128 144)
(define-color +DarkSlateGray+         47  79  79)
(define-color +Black+                 0   0   0)

;;;; My custom colors
(define-color +Moon+                  200 234 223)
(define-color +Space+                 40  30  40)
