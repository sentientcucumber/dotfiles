(deftheme ash
  "Experimental theme") 

(require 'color)

(defun ash-color-alist (hue hex)
  "Create a color, storing the HUE and HEX values."
  (let ((h (* hue 360.0)))
    `((hue . ,h)
      (hex . ,hex))))

(defun ash-hsl-to-hex (h s l)
  "Create a hexadecimal representation of hue, saturation, and
luminosity values. H should be between 0 to 360 (inclusive),
and S and L should be between 0 and 100 (inclusive)."
  (let* ((hue (/ h 360.0))
         (sat (/ s 100.0))
         (lum (/ l 100.0))
         (hex (apply 'color-rgb-to-hex
                     (color-hsl-to-rgb hue sat lum))))
    (ash-color-alist hue hex)))

(defun ash-mix-colors (base mix weight)
  "Combine the BASE color with the MIX color. WEIGHT determines
the amount of the MIX color to combine with BASE."
  (let* ((p (/ weight 100.0))
         (w (- (* p 2) 1))
         (alpha 1)
         (w1 (if (= (* w a) -1)
                 w
               (/ (+ w a)
                  (+ 1 (+ 1 (* w a)))
                  2.0)))
         (w2 (- 1 w1)))))

(defun ash-high-hue-p (color)
  "Check if COLOR has a high hue value. COLOR should be an
ash-color-list."
  (let ((hue (cdr (assoc 'hue color))))
    (and (> hue 30)
         (< hue 140))))

(defun ash-cool-color-p (color)
  "Check if COLOR is a cool color. COLOR should be an
ash-color-list."
  (let ((hue (cdr (assoc 'hue color))))
    (and (< hue 300)
         (> hue 120))))

(defvar ash-primary-color
  (ash-hsl-to-hex 0 100 50)
  "The primary color for the theme.")

(defvar ash-colors-alist
  '(("ash-darkest"    . "#292929")
    ("ash-brightest"  . "#ebebeb")
    ("ash-primary"    . "#ffffff")
    ("ash-complement" . "#ffffff"))
  "List of colors used in the `ash-theme'")

;; Pulled this from `zenburn-theme' so I can reference predefined
;; variables.
(defmacro ash-theme-with-color-variables (&rest body)
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   ash-colors-alist))
     ,@body))

(ash-theme-with-color-variables
 (custom-theme-set-faces
  'ash
  `(default ((t (:foreground ,ash-darkest :background ,ash-brightest))))
  `(font-lock-keyword-face ((t (:foreground ,ash-primary))))))

(provide-theme 'ash)
