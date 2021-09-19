(define-module (engstrand configs johan)
               #:use-module (rde features base)
               #:use-module (rde features gnupg)
               #:use-module (engstrand features browsers)
               #:use-module (engstrand features emacs)
               #:use-module (engstrand features virtualization)
               #:use-module (engstrand features wayland)
               #:use-module (engstrand utils)
               #:use-module (engstrand configs))

; TODO: Add custom packages as feature:
;       "youtube-dl"

(define-public %user-features
               (append
                 (list
                   (feature-user-info
                     #:user-name "johan"
                     #:full-name "Johan Engstrand"
                     #:email "johan@engstrand.nu")
                   (feature-gnupg
                     #:gpg-primary-key "DFC6C6B70EF5F7CB75EE97E6DF3088DDBCAD566D"
                     #:pinentry-flavor 'gtk2
                     #:gpg-smart-card? #f)
                   (feature-qutebrowser
                     #:add-keybindings? #f)
                   (feature-wayland-wbg
                     #:path (string-append (getenv "HOME")
                                           "/engstrand-config/wallpapers/default.jpg")))
                 %engstrand-emacs-base-features
                 %engstrand-base-features))
