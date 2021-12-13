(define-module (engstrand features sync)
               #:use-module (rde features)
               #:use-module (rde features predicates)
               #:use-module (engstrand utils)
               #:use-module (guix gexp)
               #:use-module (guix packages)
               #:use-module (gnu packages kde)
               #:use-module (gnu services)
               #:use-module (gnu home services)
               #:use-module (gnu home services shepherd)
               #:export (feature-kdeconnect))

(define* (feature-kdeconnect)
         ; TODO: add key for phone ID?
         "Install and configure KDE Connect."

         (define (get-home-services config)
           "Return a list of home services required by KDE Connect."
           (let ((has-dwl-guile? (get-value 'dwl-guile config)))
             (make-service-list
               (simple-service
                 'kdeconnect-add-home-packages-to-profile
                 home-profile-service-type
                 (list kdeconnect))
               (simple-service
                 'kdeconnect-add-shepherd-daemons
                 home-shepherd-service-type
                 (list
                   (shepherd-service
                     (documentation "Run the KDE Connect daemon.")
                     (requirement (append '(dbus-home)
                                          (if has-dwl-guile? '(dwl-guile) '())))
                     (provision '(kdeconnect))
                     (auto-start? #t)
                     (respawn? #t)
                     (start #~(make-forkexec-constructor
                                (list #$(file-append kdeconnect "/libexec/kdeconnectd"))))
                     (stop  #~(make-kill-destructor))))))))

         (feature
           (name 'kdeconnect)
           (home-services-getter get-home-services)))
