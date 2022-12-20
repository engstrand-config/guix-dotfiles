;; Adapted from the `kurecolor-interpolate' function of kurecolor.el
(defun my-modus-themes-interpolate (color1 color2)
  (cl-destructuring-bind (r g b)
      (mapcar #'(lambda (n) (* (/ n 2) 255.0))
              (cl-mapcar '+ (color-name-to-rgb color1) (color-name-to-rgb color2)))
    (format "#%02X%02X%02X" r g b)))

(defun my-modus-themes-tint-palette (palette bg-blend fg-blend)
  "Modify Modus PALETTE programmatically and return a new palette.
Blend background colors with BG-BLEND and foreground colors with FG-BLEND."
  (let (name cons colors)
    (dolist (cons palette)
      (let* ((color-name (symbol-name (car cons)))
             (blend (if (string-match "bg-" color-name) bg-blend fg-blend)))
        (if (or (string-match "bg-" color-name)
                (string-match "fg-" color-name))
            (setq name (my-modus-themes-interpolate (cdr cons) blend))
            (setq name (cdr cons))))
      (setq name (format "%s" name))
      (setq cons `(,(car cons) . ,name))
      (push cons colors))
    colors))
