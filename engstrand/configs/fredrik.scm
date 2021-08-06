(define-module (engstrand configs fredrik)
               #:use-module (rde features)
               #:use-module (rde features base)
               #:use-module (rde features gnupg)
               #:use-module (rde features keyboard)
               #:use-module (rde features version-control)
               #:use-module (dwl-guile home-service)
               #:use-module (engstrand configs)
               #:use-module (engstrand features nix)
               #:use-module (engstrand features state)
               #:use-module (engstrand features wayland))

(define-public %user-features
               (list
                 (feature-user-info
                   #:user-name "fredrik"
                   #:full-name "Fredrik Engstrand"
                   #:email "fredrik@engstrand.nu")
                 (feature-gnupg
                   #:gpg-primary-key "C9BEB8A04458FDDF12681B39029D8EB77E18D68C"
                   #:gpg-smart-card? #f)
                 (feature-git
                   #:sign-commits? #t)
                 (feature-state-git
                   #:repos
                   `(("repos/pywalfox" .
                      "git@github.com:frewacom/pywalfox.git")
                     ("repos/pywalfox-native" .
                      "git@github.com:frewacom/pywalfox-native.git")))
                 (feature-nix)
                 (feature-wayland-dwl-guile
                   #:dwl-guile-configuration
                   (home-dwl-guile-configuration
                     (patches %engstrand-dwl-guile-patches)
                     (config
                       (dwl-config
                         (inherit %engstrand-dwl-guile-config)))))
                 (feature-wayland-bemenu)
                 (feature-wayland-foot)))
