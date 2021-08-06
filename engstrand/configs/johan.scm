(define-module (engstrand configs johan)
               #:use-module (rde features)
               #:use-module (rde features base)
               #:use-module (rde features gnupg)
               #:use-module (rde features keyboard)
               #:use-module (rde features version-control)
               #:use-module (dwl-guile home-service)
               #:use-module (engstrand configs)
               #:use-module (engstrand features nix)
               #:use-module (engstrand features wayland))

; TODO: Add custom packages as feature:
;       "youtube-dl"

(define-public %user-features
               (list
                 (feature-user-info
                   #:user-name "johan"
                   #:full-name "Johan Engstrand"
                   #:email "johan@engstrand.nu")
                 (feature-gnupg
                   #:gpg-primary-key "DFC6C6B70EF5F7CB75EE97E6DF3088DDBCAD566D"
                   #:gpg-smart-card? #f)
                 (feature-git
                   #:sign-commits? #t)
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

