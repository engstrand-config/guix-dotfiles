(define-module (engstrand features utils)
               #:use-module (rde features)
               #:use-module (rde features predicates)
               #:use-module (guix gexp)
               #:use-module (gnu packages gnupg)
               #:use-module (gnu services)
               #:use-module (gnu home services)
               #:use-module (gnu home services shepherd)
               #:use-module (engstrand utils)
               #:use-module (engstrand packages rust)
               #:use-module (engstrand packages utils)
               #:export (feature-imv
                          feature-bitwarden-cli))

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

(define* (feature-bitwarden-cli
           #:key
           (email #f)
           (pinentry pinentry-gtk2) ;; pinentry-bemenu does not work
           (lock-timeout 300)) ;; 5 minutes
         "Setup rbw, the unofficial Bitwarden CLI."

         (ensure-pred package? pinentry)
         (ensure-pred maybe-string? email)
         (ensure-pred number? lock-timeout)

         (define (get-home-services config)
           "Return a list of home services required by rbw"
           (list
             (simple-service
               'add-rbw-home-packages-to-profile
               home-profile-service-type
               (list rbw))
             ;; rbw mutates the configuration file to set a unique device-id
             ;; upon registering the device using your API key.
             ;; See https://github.com/doy/rbw/issues/74.
             ;;
             ;; To circumvent this, a separate config file is saved, allowing us
             ;; to listen for changes and mutate the real config file.
             (simple-service
               'create-rbw-config
               home-files-service-type
               `(("config/rbw/guix-watcher"
                  ,(mixed-text-file
                     "guix-watcher"
                     email (number->string lock-timeout) pinentry))))
             (simple-service
               'on-rbw-config-change
               home-run-on-change-service-type
               (let ((bin (file-append rbw "/bin/rbw")))
                 `(("files/config/rbw/guix-watcher"
                    ,#~(begin
                         (when #$email
                           (system* #$bin "config" "set" "email" #$email))
                         (system* #$bin "config" "set" "pinentry"
                                  #$(file-append pinentry "/bin/pinentry-gtk-2"))
                         (system* #$bin "config" "set" "lock_timeout"
                                  #$(number->string lock-timeout)))))))
             (simple-service
               'add-rbw-initial-setup-shepherd-service
               home-shepherd-service-type
               (list
                 (shepherd-service
                   (documentation "Run initial setup for rbw (Bitwarden CLI).")
                   (provision '(rbw-setup))
                   (requirement '())
                   (auto-start? #f)
                   (respawn? #f)
                   (start
                     #~(make-forkexec-constructor
                         (list #$(file-append rbw "/bin/rbw") "register")))
                   (stop #~(make-kill-destructor)))))))

         (feature
           (name 'bitwarden-cli)
           (home-services-getter get-home-services)))
