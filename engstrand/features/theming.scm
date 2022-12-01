(define-module (engstrand features theming)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (rde features)
  #:use-module (farg config)
  #:use-module (farg colorscheme)
  #:use-module (farg home-service)
  #:use-module (engstrand wallpapers)
  #:export (
            %engstrand-default-farg-config
            engstrand-farg-palette
            feature-farg))

(define (engstrand-farg-palette palette)
  (lambda (name)
    (match name
      ('0-text (make-readable (palette 0) (palette 'background)))
      ('1-text (make-readable (palette 1) (palette 'background)))
      ('2-text (make-readable (palette 2) (palette 'background)))
      ('3-text (make-readable (palette 3) (palette 'background)))
      ('4-text (make-readable (palette 4) (palette 'background)))
      ('5-text (make-readable (palette 5) (palette 'background)))
      ('6-text (make-readable (palette 6) (palette 'background)))
      ('red (make-readable (blend "#FF0000" (palette 'primary) 0.7)
                           (palette 'background)))
      ('green (make-readable (blend "#00FF00" (palette 'primary) 0.7)
                             (palette 'background)))
      ;; Redirect any other lookups to the default palette
      (_ (palette name)))))

(define %engstrand-default-farg-config
  (let ((default-wallpaper (get-wallpaper-path "default.jpg")))
    (farg-config
     ;; TODO: Add root wallpaper directory as variable in wallpapers channel
     (alpha (lambda (light?) (if light? 0.98 0.9)))
     (wallpaper-search-directory (dirname default-wallpaper))
     (palette-getter engstrand-farg-palette)
     (wallpaper default-wallpaper))))

(define* (feature-farg)
  "Installs and configures farg, a system colorscheme manager for Guix."

  (lambda (home-farg-config palette)
    (define (get-home-services config)
      "Return a list of home services required by farg"
      (list
       ;; The home service configuration should not be created directly.
       ;; Instead, create your own farg-config in your user and it will
       ;; automatically be included.
       (service home-farg-service-type home-farg-config)))

    (feature
     (name 'farg)
     (values '(("farg" . #t)))
     (home-services-getter get-home-services))))
