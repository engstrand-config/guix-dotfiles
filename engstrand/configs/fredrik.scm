(define-module (engstrand configs fredrik)
               #:use-module (rde features)
               #:use-module (rde features base)
               #:use-module (rde features gnupg)
               #:use-module (engstrand utils)
               #:use-module (engstrand configs)
               #:use-module (engstrand features xorg)
               #:use-module (engstrand features state)
               #:use-module (engstrand features browsers)
               #:use-module (engstrand features virtualization)
               #:use-module (engstrand features wayland))

(define-public %user-features
               (append
                 (list
                   (feature-user-info
                     #:user-name "fredrik"
                     #:full-name "Fredrik Engstrand"
                     #:email "fredrik@engstrand.nu")
                   ; TODO: Add custom gnupg feature. We should be using pinentry-bemenu instead.
                   ;       However, home-gnupg-service will always append "/bin/pinentry" to the
                   ;       pinentry exectuable in ~/.gnupg/gpg-agent.conf. pinentry-bemenu does not
                   ;       have such an exectuable. The bemenu exectuable is called "pinentry-bemenu".
                   ;
                   ;       This leaves us with two options; add support for it in the home-service,
                   ;       or transform the pinentry-bemenu package and move the pinentry exectuable to "/bin/pinentry".
                   (feature-gnupg
                     #:gpg-primary-key "C9BEB8A04458FDDF12681B39029D8EB77E18D68C"
                     #:pinentry-flavor 'gtk2
                     #:gpg-smart-card? #f)
                   ; (feature-state-git
                   ;   #:repos
                   ;   `(("repos/pywalfox" . "git@github.com:frewacom/pywalfox.git")
                   ;     ("repos/pywalfox-native" . "git@github.com:frewacom/pywalfox-native.git")))
                   (feature-xorg-dwm)
                   (feature-virtualization)
                   (feature-qutebrowser)
                   (feature-wayland-wbg
                     #:path (string-append (getenv "HOME")
                                           "/engstrand-config/wallpapers/default.jpg")))
                 ; TODO: When Firefox feature is done, add it, but set (add-keybindings? #f)
                 (modify-features %engstrand-base-features
                                  (delete 'firefox))))
