(define-module (engstrand features utils)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (guix gexp)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu services)
  #:use-module (gnu services dbus)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (engstrand utils)
  #:use-module (engstrand packages utils)
  #:use-module (engstrand home-services qutebrowser)
  #:export (feature-imv
            feature-rbw
            feature-rbw-qutebrowser
            feature-piper))

(define* (feature-imv)
  "Setup imv, an image viewer for X11 and Wayland."

  ;; TODO: Add configuration in Guile

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

(define* (feature-piper)
  "Set up Piper, a configuration utility for gaming mice."

  (define (get-home-services config)
    "Return a list of home services required by Piper"
    (list
     (simple-service
      'add-piper-home-packages-to-profile
      home-profile-service-type
      ;; seems like `python' is needed also
      (pkgs '("python" "piper")))))

  (define (get-system-services config)
    "Return a list of system services required by Piper"
    (list
     (simple-service
      'ratbagd
      dbus-root-service-type
      (list libratbag))))

  (feature
   (name 'piper)
   (home-services-getter get-home-services)
   (system-services-getter get-system-services)))

(define* (feature-rbw
          #:key
          (package rbw-latest)
          (email #f)
          (pinentry (file-append pinentry-bemenu "/bin/pinentry-bemenu"))
          (lock-timeout 300)) ;; 5 minutes
  "Setup rbw, the unofficial Bitwarden CLI."

  (ensure-pred package? package)
  (ensure-pred file-like? pinentry)
  (ensure-pred maybe-string? email)
  (ensure-pred number? lock-timeout)

  (define (get-home-services config)
    "Return a list of home services required by rbw"
    (list
     (simple-service
      'add-rbw-home-packages-to-profile
      home-profile-service-type
      (list package))
     ;; rbw mutates the configuration file to set a unique device-id
     ;; upon registering the device using your API key.
     ;; See https://github.com/doy/rbw/issues/74.
     ;;
     ;; To circumvent this, a separate config file is saved, allowing us
     ;; to listen for changes and mutate the real config file.
     (simple-service
      'create-rbw-config
      home-files-service-type
      `((".config/rbw/immutable-config"
         ,(mixed-text-file
           "rbw-immutable-config"
           email (number->string lock-timeout) pinentry))))
     (simple-service
      'on-rbw-config-change
      home-run-on-change-service-type
      (let ((bin (file-append package "/bin/rbw")))
        `(("files/.config/rbw/immutable-config"
           ,#~(begin
                (when #$email
                  (system* #$bin "config" "set" "email" #$email))
                (system* #$bin "config" "set" "pinentry"
                         #$pinentry)
                (system* #$bin "config" "set" "lock_timeout"
                         #$(number->string lock-timeout)))))))))

  (feature
   (name 'rbw)
   (home-services-getter get-home-services)))

(define* (feature-rbw-qutebrowser)
  "Add a userscript to qutebrowser for adding auto-fill using rbw."

  (define (get-home-services config)
    (list
     (simple-service
      'qutebrowser-rbw-autofill
      home-qutebrowser-service-type
      (home-qutebrowser-extension
       (bindings
        '(("<Ctrl-Shift-i>" "spawn --userscript qute-rbw" "insert")
          ("<Ctrl-Shift-p>" "spawn --userscript qute-rbw --password-only" "insert")
          ("<Ctrl-Shift-u>" "spawn --userscript qute-rbw --username-only" "insert")
          ("<Ctrl-Shift-i>" "spawn --userscript qute-rbw" "normal")
          ("<Ctrl-Shift-p>" "spawn --userscript qute-rbw --password-only" "normal")
          ("<Ctrl-Shift-u>" "spawn --userscript qute-rbw --username-only" "normal")))
       (userscripts
        `(("qute-rbw" . ,(local-file "../files/qute-rbw"))))))))

  (feature
   (name 'rbw-qutebrowser)
   (home-services-getter get-home-services)))
