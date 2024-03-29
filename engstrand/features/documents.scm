(define-module (engstrand features documents)
  #:use-module (guix gexp)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (gnu packages pdf)
  #:use-module (gnu services)
  #:use-module (guix packages)
  #:use-module (gnu home services)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages python-xyz)
  #:use-module (engstrand utils)
  #:use-module (engstrand packages documents)
  #:export (
            feature-zathura
            feature-sioyek
            feature-latex))

;; NOTE: zathura plugins uses the ZATHURA_PLUGINS_PATH environment variable
;;       for linking to installed plugins. Therefore, you will need to restart
;;       the session after installing for the plugin to be loaded correctly.
(define* (feature-zathura
          #:key
          (default-reader? #f)
          (zathura-pdf-plugin zathura-pdf-mupdf))
  "Setup zathura, a minimal document viewer."

  (ensure-pred boolean? default-reader?)
  (ensure-pred package? zathura-pdf-plugin)

  (define (get-home-services config)
    "Return a list of system services required by zathura"
    (list
     (when default-reader?
       (simple-service
        'set-zathura-environment-variable
        home-environment-variables-service-type
        `(("READER" . ,(file-append zathura "/bin/zathura")))))
     (simple-service
      'add-zathura-home-packages-to-profile
      home-profile-service-type
      (list zathura zathura-pdf-plugin))))

  (feature
   (name 'zathura)
   (home-services-getter get-home-services)))

(define* (feature-sioyek
          #:key
          (default-reader? #f))
  "Setup zathura, a minimal document viewer."

  (ensure-pred boolean? default-reader?)

  (define (get-home-services config)
    "Return a list of system services required by sioyek"

    (define package
      (if (get-value 'wayland config)
          sioyek/wayland
          sioyek))

    (list
     (when default-reader?
       (simple-service
        'set-sioyek-environment-variable
        home-environment-variables-service-type
        `(("READER" . ,(file-append package "/bin/sioyek")))))
     (simple-service
      'add-sioyek-home-packages-to-profile
      home-profile-service-type
      (list package))))

  (feature
   (name 'sioyek)
   (home-services-getter get-home-services)))

(define* (feature-latex)
  (define (get-home-services config)
    (list
     (simple-service
      'add-latex-home-packages-to-profile
      home-profile-service-type
      (list texlive-base
            texlive-listings
            texlive-hyperref
            texlive-wrapfig
            texlive-amsmath
            texlive-amsfonts
            texlive-url
            texlive-xcolor
            texlive-tcolorbox
            texlive-minted
            texlive-caption
            python-pygments))))

  (feature
   (name 'latex)
   (values '((latex . #t)))
   (home-services-getter get-home-services)))
