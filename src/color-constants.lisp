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
  (:export ;; Html Color Constants
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

;;; COLOR CONSTANTS
(defmacro define-color (name r g b)
  `(defconstant ,name (clgfw:make-color ,r ,g ,b)))

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
