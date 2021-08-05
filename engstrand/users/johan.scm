(define-module (engstrand users johan)
               #:use-module (rde features)
               #:use-module (rde features base)
               #:use-module (rde features gnupg)
               #:use-module (rde features keyboard)
               #:use-module (rde features version-control)
               #:use-module (engstrand defaults))

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
                   #:sign-commits? #t)))
