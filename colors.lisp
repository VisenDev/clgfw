;;;; COLOR.LISP
;;;;
;;;; DEFINES A BUNCH OF NAMED COLOR CONSTANTS
;;;;
;;;; COPYRIGHT 2026
;;;; ROBERT BURNETT
;;;; LICENSED UNDER APACHE-2.0

(defpackage #:clgfw/color
  (:use #:cl #:clgfw)
  (:export +IndianRed+ +LightCoral+ +Salmon+ +DarkSalmon+          
           +LightSalmon+ +Crimson+ +Red+ +FireBrick+           
           +DarkRed+ +Pink+ +LightPink+ +HotPink+             
           +DeepPink+ +MediumVioletRed+ +PaleVioletRed+ +LightSalmon+         
           +Coral+ +Tomato+ +OrangeRed+ +DarkOrange+          
           +Orange+ +Gold+ +Yellow+ +LightYellow+         
           +LemonChiffon+ +LightGoldenrodYellow+ +PapayaWhip+ +Moccasin+            
           +PeachPuff+ +PaleGoldenrod+ +Khaki+ +DarkKhaki+           
           +Lavender+ +Thistle+ +Plum+ +Violet+              
           +Orchid+ +Fuchsia+ +Magenta+ +MediumOrchid+        
           +MediumPurple+ +RebeccaPurple+ +BlueViolet+ +DarkViolet+          
           +DarkOrchid+ +DarkMagenta+ +Purple+ +Indigo+              
           +SlateBlue+ +DarkSlateBlue+ +MediumSlateBlue+ +GreenYellow+         
           +Chartreuse+ +LawnGreen+ +Lime+ +LimeGreen+           
           +PaleGreen+ +LightGreen+ +MediumSpringGreen+ +SpringGreen+         
           +MediumSeaGreen+ +SeaGreen+ +ForestGreen+ +Green+               
           +DarkGreen+ +YellowGreen+ +OliveDrab+ +Olive+               
           +DarkOliveGreen+ +MediumAquamarine+ +DarkSeaGreen+ +LightSeaGreen+       
           +DarkCyan+ +Teal+ +Aqua+ +Cyan+                
           +LightCyan+ +PaleTurquoise+ +Aquamarine+ +Turquoise+           
           +MediumTurquoise+ +DarkTurquoise+ +CadetBlue+ +SteelBlue+           
           +LightSteelBlue+ +PowderBlue+ +LightBlue+ +SkyBlue+             
           +LightSkyBlue+ +DeepSkyBlue+ +DodgerBlue+ +CornflowerBlue+      
           +MediumSlateBlue+ +RoyalBlue+ +Blue+ +MediumBlue+          
           +DarkBlue+ +Navy+ +MidnightBlue+ +Cornsilk+            
           +BlanchedAlmond+ +Bisque+ +NavajoWhite+ +Wheat+               
           +BurlyWood+ +Tan+ +RosyBrown+ +SandyBrown+          
           +Goldenrod+ +DarkGoldenrod+ +Peru+ +Chocolate+           
           +SaddleBrown+ +Sienna+ +Brown+ +Maroon+              
           +White+ +Snow+ +HoneyDew+ +MintCream+           
           +Azure+ +AliceBlue+ +GhostWhite+ +WhiteSmoke+          
           +SeaShell+ +Beige+ +OldLace+ +FloralWhite+         
           +Ivory+ +AntiqueWhite+ +Linen+ +LavenderBlush+       
           +MistyRose+ +Gainsboro+ +LightGray+ +Silver+              
           +DarkGray+ +Gray+ +DimGray+ +LightSlateGray+      
           +SlateGray+ +DarkSlateGray+ +Black+

           ;; My custom colors
           +Moon+ +Space+
           ))

(in-package #:clgfw/color)

;;; HTML 5 COLORS
(defconstant +IndianRed+            (make-color 205 92  92))
(defconstant +LightCoral+           (make-color 240 128 128))
(defconstant +Salmon+               (make-color 250 128 114))
(defconstant +DarkSalmon+           (make-color 233 150 122))
(defconstant +LightSalmon+          (make-color 255 160 122))
(defconstant +Crimson+              (make-color 220 20  60))
(defconstant +Red+                  (make-color 255 0   0))
(defconstant +FireBrick+            (make-color 178 34  34))
(defconstant +DarkRed+              (make-color 139 0   0))
(defconstant +Pink+                 (make-color 255 192 203))
(defconstant +LightPink+            (make-color 255 182 193))
(defconstant +HotPink+              (make-color 255 105 180))
(defconstant +DeepPink+             (make-color 255 20  147))
(defconstant +MediumVioletRed+      (make-color 199 21  133))
(defconstant +PaleVioletRed+        (make-color 219 112 147))
(defconstant +LightSalmon+          (make-color 255 160 122))
(defconstant +Coral+                (make-color 255 127 80))
(defconstant +Tomato+               (make-color 255 99  71))
(defconstant +OrangeRed+            (make-color 255 69  0))
(defconstant +DarkOrange+           (make-color 255 140 0))
(defconstant +Orange+               (make-color 255 165 0))
(defconstant +Gold+                 (make-color 255 215 0))
(defconstant +Yellow+               (make-color 255 255 0))
(defconstant +LightYellow+          (make-color 255 255 224))
(defconstant +LemonChiffon+         (make-color 255 250 205))
(defconstant +LightGoldenrodYellow+ (make-color 250 250 210))
(defconstant +PapayaWhip+           (make-color 255 239 213))
(defconstant +Moccasin+             (make-color 255 228 181))
(defconstant +PeachPuff+            (make-color 255 218 185))
(defconstant +PaleGoldenrod+        (make-color 238 232 170))
(defconstant +Khaki+                (make-color 240 230 140))
(defconstant +DarkKhaki+            (make-color 189 183 107))
(defconstant +Lavender+             (make-color 230 230 250))
(defconstant +Thistle+              (make-color 216 191 216))
(defconstant +Plum+                 (make-color 221 160 221))
(defconstant +Violet+               (make-color 238 130 238))
(defconstant +Orchid+               (make-color 218 112 214))
(defconstant +Fuchsia+              (make-color 255 0   255))
(defconstant +Magenta+              (make-color 255 0   255))
(defconstant +MediumOrchid+         (make-color 186 85  211))
(defconstant +MediumPurple+         (make-color 147 112 219))
(defconstant +RebeccaPurple+        (make-color 102 51  153))
(defconstant +BlueViolet+           (make-color 138 43  226))
(defconstant +DarkViolet+           (make-color 148 0   211))
(defconstant +DarkOrchid+           (make-color 153 50  204))
(defconstant +DarkMagenta+          (make-color 139 0   139))
(defconstant +Purple+               (make-color 128 0   128))
(defconstant +Indigo+               (make-color 75  0   130))
(defconstant +SlateBlue+            (make-color 106 90  205))
(defconstant +DarkSlateBlue+        (make-color 72  61  139))
(defconstant +MediumSlateBlue+      (make-color 123 104 238))
(defconstant +GreenYellow+          (make-color 173 255 47))
(defconstant +Chartreuse+           (make-color 127 255 0))
(defconstant +LawnGreen+            (make-color 124 252 0))
(defconstant +Lime+                 (make-color 0   255 0))
(defconstant +LimeGreen+            (make-color 50  205 50))
(defconstant +PaleGreen+            (make-color 152 251 152))
(defconstant +LightGreen+           (make-color 144 238 144))
(defconstant +MediumSpringGreen+    (make-color 0   250 154))
(defconstant +SpringGreen+          (make-color 0   255 127))
(defconstant +MediumSeaGreen+       (make-color 60  179 113))
(defconstant +SeaGreen+             (make-color 46  139 87))
(defconstant +ForestGreen+          (make-color 34  139 34))
(defconstant +Green+                (make-color 0   128 0))
(defconstant +DarkGreen+            (make-color 0   100 0))
(defconstant +YellowGreen+          (make-color 154 205 50))
(defconstant +OliveDrab+            (make-color 107 142 35))
(defconstant +Olive+                (make-color 128 128 0))
(defconstant +DarkOliveGreen+       (make-color 85  107 47))
(defconstant +MediumAquamarine+     (make-color 102 205 170))
(defconstant +DarkSeaGreen+         (make-color 143 188 139))
(defconstant +LightSeaGreen+        (make-color 32  178 170))
(defconstant +DarkCyan+             (make-color 0   139 139))
(defconstant +Teal+                 (make-color 0   128 128))
(defconstant +Aqua+                 (make-color 0   255 255))
(defconstant +Cyan+                 (make-color 0   255 255))
(defconstant +LightCyan+            (make-color 224 255 255))
(defconstant +PaleTurquoise+        (make-color 175 238 238))
(defconstant +Aquamarine+           (make-color 127 255 212))
(defconstant +Turquoise+            (make-color 64  224 208))
(defconstant +MediumTurquoise+      (make-color 72  209 204))
(defconstant +DarkTurquoise+        (make-color 0   206 209))
(defconstant +CadetBlue+            (make-color 95  158 160))
(defconstant +SteelBlue+            (make-color 70  130 180))
(defconstant +LightSteelBlue+       (make-color 176 196 222))
(defconstant +PowderBlue+           (make-color 176 224 230))
(defconstant +LightBlue+            (make-color 173 216 230))
(defconstant +SkyBlue+              (make-color 135 206 235))
(defconstant +LightSkyBlue+         (make-color 135 206 250))
(defconstant +DeepSkyBlue+          (make-color 0   191 255))
(defconstant +DodgerBlue+           (make-color 30  144 255))
(defconstant +CornflowerBlue+       (make-color 100 149 237))
(defconstant +MediumSlateBlue+      (make-color 123 104 238))
(defconstant +RoyalBlue+            (make-color 65  105 225))
(defconstant +Blue+                 (make-color 0   0   255))
(defconstant +MediumBlue+           (make-color 0   0   205))
(defconstant +DarkBlue+             (make-color 0   0   139))
(defconstant +Navy+                 (make-color 0   0   128))
(defconstant +MidnightBlue+         (make-color 25  25  112))
(defconstant +Cornsilk+             (make-color 255 248 220))
(defconstant +BlanchedAlmond+       (make-color 255 235 205))
(defconstant +Bisque+               (make-color 255 228 196))
(defconstant +NavajoWhite+          (make-color 255 222 173))
(defconstant +Wheat+                (make-color 245 222 179))
(defconstant +BurlyWood+            (make-color 222 184 135))
(defconstant +Tan+                  (make-color 210 180 140))
(defconstant +RosyBrown+            (make-color 188 143 143))
(defconstant +SandyBrown+           (make-color 244 164 96))
(defconstant +Goldenrod+            (make-color 218 165 32))
(defconstant +DarkGoldenrod+        (make-color 184 134 11))
(defconstant +Peru+                 (make-color 205 133 63))
(defconstant +Chocolate+            (make-color 210 105 30))
(defconstant +SaddleBrown+          (make-color 139 69  19))
(defconstant +Sienna+               (make-color 160 82  45))
(defconstant +Brown+                (make-color 165 42  42))
(defconstant +Maroon+               (make-color 128 0   0))
(defconstant +White+                (make-color 255 255 255))
(defconstant +Snow+                 (make-color 255 250 250))
(defconstant +HoneyDew+             (make-color 240 255 240))
(defconstant +MintCream+            (make-color 245 255 250))
(defconstant +Azure+                (make-color 240 255 255))
(defconstant +AliceBlue+            (make-color 240 248 255))
(defconstant +GhostWhite+           (make-color 248 248 255))
(defconstant +WhiteSmoke+           (make-color 245 245 245))
(defconstant +SeaShell+             (make-color 255 245 238))
(defconstant +Beige+                (make-color 245 245 220))
(defconstant +OldLace+              (make-color 253 245 230))
(defconstant +FloralWhite+          (make-color 255 250 240))
(defconstant +Ivory+                (make-color 255 255 240))
(defconstant +AntiqueWhite+         (make-color 250 235 215))
(defconstant +Linen+                (make-color 250 240 230))
(defconstant +LavenderBlush+        (make-color 255 240 245))
(defconstant +MistyRose+            (make-color 255 228 225))
(defconstant +Gainsboro+            (make-color 220 220 220))
(defconstant +LightGray+            (make-color 211 211 211))
(defconstant +Silver+               (make-color 192 192 192))
(defconstant +DarkGray+             (make-color 169 169 169))
(defconstant +Gray+                 (make-color 128 128 128))
(defconstant +DimGray+              (make-color 105 105 105))
(defconstant +LightSlateGray+       (make-color 119 136 153))
(defconstant +SlateGray+            (make-color 112 128 144))
(defconstant +DarkSlateGray+        (make-color 47  79  79))
(defconstant +Black+                (make-color 0   0   0))

;;;; My custom colors
(defconstant +Moon+ (make-color 200 234 223))
(defconstant +Space+ (make-color 40 30 40))
