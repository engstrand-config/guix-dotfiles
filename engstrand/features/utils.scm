(define-module (engstrand features utils)
               #:use-module (rde features)
               #:use-module (rde features predicates)
               #:use-module (gnu services)
               #:use-module (gnu home services)
               #:use-module (engstrand utils)
               #:export (feature-imv))

(define* (feature-imv)
         "Setup imv, an image viewer for X11 and Wayland."

         ; TODO: Add configuration in Guile

         (define (get-home-services config)
           "Return a list of home services required by imv"
           (list
             (simple-service
               'add-imv-home-packages-to-profile
               home-profile-service-type
               (pkgs '("imv")))))

         (feature
           (name 'imv)
           (home-services-getter get-home-services)))
