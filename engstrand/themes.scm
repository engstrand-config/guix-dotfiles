(define-module (engstrand themes)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (farg theme)
  #:use-module (farg colors)
  #:use-module (farg source)
  #:use-module (engstrand wallpapers)
  #:export (
            engstrand-theme-light
            engstrand-theme-dark
            engstrand-farg-palette))

(define engstrand-theme-light
  (farg-source
   (theme
    (farg-theme
     (light? #t)
     (alpha 0.98)
     (fg "#000000")
     (bg "#FFFFFF")
     (bg-alt "#F0F0F0")
     (accent-0 "#531AB6")
     (accent-1 "#721045")
     (accent-2 "#005F5F")
     (accent-3 "#0000B0")
     (accent-4 "#A0132F")
     (accent-5 "#8F0075")
     (accent-6 "#3548CF")
     (accent-7 "#2A5045")))))

(define engstrand-theme-dark
  (farg-source
   (theme
    (farg-theme
     (light? #f)
     (alpha 0.96)
     (fg "#FFFFFF")
     (bg "#000000")
     (bg-alt "#1E1E1E")
     (accent-0 "#B6A0FF")
     (accent-1 "#FEACD0")
     (accent-2 "#6AE4B9")
     (accent-3 "#00BCFF")
     (accent-4 "#FF7F9F")
     (accent-5 "#F78FE7")
     (accent-6 "#79A8FF")
     (accent-7 "#9AC8E0")))))

(define (engstrand-farg-palette palette)
  (define red
    (farg:make-readable (farg:blend "#FF0000" (palette 'accent-0) 0.7)
                        (palette 'bg)
                        5))
  (define green
    (farg:make-readable (farg:blend "#00FF00" (palette 'accent-0) 0.7)
                        (palette 'bg)
                        5))
  (define blue
    (farg:make-readable (farg:blend "#0000FF" (palette 'accent-0) 0.7)
                        (palette 'bg)
                        5))
  (define yellow
    (farg:make-readable (farg:blend "#FFFF00" (palette 'accent-0) 0.7)
                        (palette 'bg)
                        5))
  (define magenta
    (farg:make-readable (farg:blend "#FF00FF" (palette 'accent-0) 0.7)
                        (palette 'bg)
                        5))
  (define cyan
    (farg:make-readable (farg:blend "#00FFFF" (palette 'accent-0) 0.7)
                        (palette 'bg)
                        5))

  (define red-text (farg:make-readable red red))
  (define green-text (farg:make-readable green green))
  (define blue-text (farg:make-readable blue blue))
  (define yellow-text (farg:make-readable yellow yellow))
  (define magenta-text (farg:make-readable magenta magenta))
  (define cyan-text (farg:make-readable cyan cyan))

  (define accent-0-text
    (farg:make-readable (palette 'accent-0) (palette 'bg)))
  (define accent-1-text
    (farg:make-readable (palette 'accent-1) (palette 'bg)))
  (define accent-2-text
    (farg:make-readable (palette 'accent-2) (palette 'bg)))
  (define accent-3-text
    (farg:make-readable (palette 'accent-3) (palette 'bg)))
  (define accent-4-text
    (farg:make-readable (palette 'accent-4) (palette 'bg)))
  (define accent-5-text
    (farg:make-readable (palette 'accent-5) (palette 'bg)))
  (define accent-6-text
    (farg:make-readable (palette 'accent-6) (palette 'bg)))
  (define accent-7-text
    (farg:make-readable (palette 'accent-7) (palette 'bg)))

  (lambda (name)
    (match name
      ('red red)
      ('green green)
      ('blue blue)
      ('yellow yellow)
      ('magenta magenta)
      ('cyan cyan)
      ('red-text red-text)
      ('green-text green-text)
      ('blue-text blue-text)
      ('yellow-text yellow-text)
      ('magenta-text magenta-text)
      ('cyan-text cyan-text)
      ('accent-0-text accent-0-text)
      ('accent-1-text accent-1-text)
      ('accent-2-text accent-2-text)
      ('accent-3-text accent-3-text)
      ('accent-4-text accent-4-text)
      ('accent-5-text accent-5-text)
      ('accent-6-text accent-6-text)
      ('accent-7-text accent-7-text)
      ;; Redirect any other lookups to the default palette
      (_ (palette name)))))
