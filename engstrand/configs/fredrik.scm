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
               #:use-module (engstrand features xorg)
               #:use-module (engstrand features video)
               #:use-module (engstrand features wayland))

; TODO: Remove xorg feature again.
(define %xorg-amdgpu-config
  "Section \"Device\"
  Identifier  \"AMD\"
  Driver      \"amdgpu\"
  Option      \"TearFree\" \"true\"
  Option      \"Backlight\" \"amdgpu_bl0\"
  EndSection")

(define-public %user-features
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
                 (feature-git
                   #:sign-commits? #t)
                 (feature-state-git
                   #:repos
                   `(("repos/pywalfox" . "git@github.com:frewacom/pywalfox.git")
                     ("repos/pywalfox-native" . "git@github.com:frewacom/pywalfox-native.git")))
                 (feature-nix)
                 (feature-mpv)
                 (feature-obs)
                 (feature-xorg-dwm
                   #:extra-config (list %xorg-amdgpu-config))
                 (feature-wayland-dwl-guile
                   #:dwl-guile-configuration
                   (home-dwl-guile-configuration
                     (patches %engstrand-dwl-guile-patches)
                     (config
                       (dwl-config
                         (inherit %engstrand-dwl-guile-config)))))
                 (feature-wayland-bemenu)
                 (feature-wayland-foot)))
