(deftheme geek
  "Experimental theme guided by https://tallys.github.io/color-theory")

;; Helper functions to create colors through HSL values instead of
;; hardcoded values. https://github.com/jolby/colors was a huge help
;; in creating these functions.

(defun geek/hsl (hue saturation lightness)
  "Define colors using HUE, SATURATION, and LIGHTNESS"
  (let* ((h (/ hue 360.0))
         (s (/ saturation 100.0))
         (l (/ lightness 100.0))
         (m2 (if (<= l 0.5)
                 (* l (1+ s))
               (- (+ l s) (* l s))))
         (m1 (- (* l 2) m2))
         (r (geek/hue-to-rgb m1 m2 (+ h (/ 1.0 3))))
         (g (geek/hue-to-rgb m1 m2 h))
         (b (geek/hue-to-rgb m1 m2 (- h (/ 1.0 3)))))
    (format "#%s%s%s" r g b)))

(defun geek/hue-to-rgb (m1 m2 hue)
  "Convert hue color to RGB components"
  (let ((h (cond
             ((< hue 0) (1+ hue))
             ((> hue 1) (1- hue))
             (t hue))))
    (geek/dec-to-hex
     (cond
       ((< (* h 6) 1) (+ m1 (* (- m2 m1) h 6)))
       ((< (* h 2) 1) m2)
       ((< (* h 3) 2) (+ m1 (* (- m2 m1) (- (/ 2.0 3) h) 6)))
       (t m1)))))
  
(defun geek/dec-to-hex (dec)
  "Convert DEC to hexadecimal format"
  (if (= dec 0)
      "00"
    (format "%x" (* 255 dec))))

(defvar geek/colors-alist
  '(("geek-brightest"  . "#fcf8f8")
    ("geek-dark"       . (geek/hsl 0 0 45))
    ("geek-darkest"    . (geek/hsl 0 0 17))
    ("geek-primary"    . (geek/hsl 200 100 50))
    ("geek-complement" . (geek/hsl 20 100 50)))
  "List of colors used throughout the theme")

;; Pulled this from the zenburn-theme package so I can reference
;; predefined variables.
(defmacro geek/theme-with-color-variables (&rest body)
  "`let' bind all colors defined in `geek/colors-alist' around BODY"
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   geek/colors-alist))
     ,@body))

(geek/theme-with-color-variables
 (custom-theme-set-faces
  'geek
  `(default ((t (:foreground ,geek-brightest :background ,geek-darkest))))
  `(font-lock-comment-face ((t (:foreground ,geek-dark))))
  `(font-lock-keyword-face ((t (:foreground ,geek-primary))))))

(provide-theme 'geek)
