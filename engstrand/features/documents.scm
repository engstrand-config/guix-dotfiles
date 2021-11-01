(define-module (engstrand features documents)
               #:use-module (rde features)
               #:use-module (rde features predicates)
               #:use-module (gnu packages pdf)
               #:use-module (gnu services)
               #:use-module (guix packages)
               #:use-module (gnu home services)
               #:use-module (gnu packages tex)
               #:use-module (gnu packages python-xyz)
               #:use-module (engstrand utils)
               #:export (
                         feature-zathura
                         feature-latex))

; NOTE: zathura plugins uses the ZATHURA_PLUGINS_PATH environment variable
;       for linking to installed plugins. Therefore, you will need to restart
;       the session after installing for the plugin to be loaded correctly.
(define* (feature-zathura
           #:key
           (zathura-pdf-plugin zathura-pdf-mupdf))
         "Setup zathura, a minimal document viewer."

         (ensure-pred package? zathura-pdf-plugin)

         (define (get-home-services config)
           "Return a list of system services required by zathura"
           (list
             (simple-service
               'add-zathura-home-packages-to-profile
               home-profile-service-type
               (list zathura zathura-pdf-plugin))))

         (feature
           (name 'zathura)
           (home-services-getter get-home-services)))

(define* (feature-latex)
         (define (get-home-services config)
           (list
             (simple-service
               'add-latex-home-packages-to-profile
               home-profile-service-type
               (list texlive texlive-latex-base python-pygments))))

         (feature
           (name 'latex)
           (home-services-getter get-home-services)))
