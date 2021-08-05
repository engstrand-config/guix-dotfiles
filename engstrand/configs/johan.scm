(define-module (engstrand configs johan)
               #:use-module (rde features)
               #:use-module (rde features base)
               #:use-module (rde features gnupg)
               #:use-module (rde features keyboard)
               #:use-module (rde features version-control))

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
                   #:sign-commits? #t)))
