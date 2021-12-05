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
           (list
             (simple-service
               'kdeconnect-add-home-packages-to-profile
               home-profile-service-type
               (pkgs '("kdeconnect")))))

           ; TODO: shepherd daemon does not work for some reason
           ; (list
           ;   (simple-service
           ;     'kdeconnect-add-shepherd-daemons
           ;     home-shepherd-service-type
           ;     (list
           ;       (shepherd-service
           ;         (requirement '(dbus-home))
           ;         (provision '(kdeconnect))
           ;         (stop  #~(make-kill-destructor))
           ;         (start #~(make-forkexec-constructor
           ;                    (list #$(file-append package "/bin/kdeconnectd")))))))))

         (feature
           (name 'kdeconnect)
           (home-services-getter get-home-services)))
