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

(define* (feature-kdeconnect
          #:key
          (device-id #f))
  "Install and configure KDE Connect."

  (ensure-pred maybe-string? device-id)

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
          (requirement (append '(dbus)
                               (if has-dwl-guile? '(dwl-guile) '())))
          (provision '(kdeconnect))
          (auto-start? #t)
          (respawn? #t)
          (start #~(make-forkexec-constructor
                    (list #$(file-append kdeconnect "/libexec/kdeconnectd"))
                    #:log-file #$(string-append (or (getenv "XDG_LOG_HOME")
                                                    (getenv "HOME"))
                                                "/kdeconnect.log")))
          (stop  #~(make-kill-destructor))))))))

  (feature
   (name 'kdeconnect)
   (values `((kdeconnect-device-id . ,device-id)))
   (home-services-getter get-home-services)))
