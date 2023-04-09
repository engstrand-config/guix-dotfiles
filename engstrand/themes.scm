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
     (fg "#000000")
     (bg "#FFFFFF")
     (bg-alt "#F0F0F0")
     (accent "#0031A9")))))

(define engstrand-theme-dark
  (farg-source
   (theme
    (farg-theme
     (light? #f)
     (fg "#FFFFFF")
     (bg "#000000")
     (bg-alt "#1E1E1E")
     (accent "#338FFF")))))

(define (engstrand-farg-palette palette)
  (define red
    (farg:make-readable (farg:blend "#FF0000" (palette 'accent) 0.7)
                        (palette 'bg)
                        5))
  (define green
    (farg:make-readable (farg:blend "#00FF00" (palette 'accent) 0.7)
                        (palette 'bg)
                        5))
  (define blue
    (farg:make-readable (farg:blend "#0000FF" (palette 'accent) 0.7)
                        (palette 'bg)
                        5))
  (define yellow
    (farg:make-readable (farg:blend "#FFFF00" (palette 'accent) 0.7)
                        (palette 'bg)
                        5))
  (define magenta
    (farg:make-readable (farg:blend "#FF00FF" (palette 'accent) 0.7)
                        (palette 'bg)
                        5))
  (define cyan
    (farg:make-readable (farg:blend "#00FFFF" (palette 'accent) 0.7)
                        (palette 'bg)
                        5))

  (define red-text (farg:make-readable red red))
  (define green-text (farg:make-readable green green))
  (define blue-text (farg:make-readable blue blue))
  (define yellow-text (farg:make-readable yellow yellow))
  (define magenta-text (farg:make-readable magenta magenta))
  (define cyan-text (farg:make-readable cyan cyan))
  (define accent-text
    (farg:make-readable (palette 'accent) (palette 'bg)))
  (define complementary-text
    (farg:make-readable (palette 'complementary) (palette 'bg)))

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
      ('accent-text accent-text)
      ('complementary-text complementary-text)
      ;; Redirect any other lookups to the default palette
      (_ (palette name)))))
