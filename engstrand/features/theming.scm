(define-module (engstrand features theming)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (rde features)
  #:use-module (farg config)
  #:use-module (farg home-service)
  #:use-module (engstrand wallpapers)
  #:export (
            %engstrand-default-farg-config
            feature-farg))

(define %engstrand-default-farg-config
  (let ((default-wallpaper (get-wallpaper-path "default.jpg")))
    (farg-config
     ;; TODO: Add root wallpaper directory as variable in wallpapers channel
     (wallpaper-search-directory (dirname default-wallpaper))
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
