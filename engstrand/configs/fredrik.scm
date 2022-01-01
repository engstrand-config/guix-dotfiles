(define-module (engstrand configs fredrik)
  #:use-module (rde features)
  #:use-module (rde features ssh)
  #:use-module (rde features base)
  #:use-module (rde features gnupg)
  #:use-module (gnu services)
  #:use-module (gnu services databases)
  #:use-module (gnu home-services ssh) ;; rde home-service
  #:use-module (dtao-guile home-service)
  #:use-module (engstrand utils)
  #:use-module (engstrand configs)
  #:use-module (engstrand features xorg)
  #:use-module (engstrand features sync)
  #:use-module (engstrand features utils)
  #:use-module (engstrand features state)
  #:use-module (engstrand features emacs)
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
    ;; TODO: Add custom gnupg feature. We should be using pinentry-bemenu instead.
    ;;       However, home-gnupg-service will always append "/bin/pinentry" to the
    ;;       pinentry exectuable in ~/.gnupg/gpg-agent.conf. pinentry-bemenu does not
    ;;       have such an exectuable. The bemenu exectuable is called "pinentry-bemenu".
    ;;
    ;;       This leaves us with two options; add support for it in the home-service,
    ;;       or transform the pinentry-bemenu package and move the pinentry exectuable to "/bin/pinentry".
    (feature-gnupg
     #:gpg-primary-key "C9BEB8A04458FDDF12681B39029D8EB77E18D68C"
     #:pinentry-flavor 'gtk2
     #:gpg-smart-card? #f)
    (feature-ssh
     #:ssh-configuration
     (home-ssh-configuration
      (extra-config
       (list
        (ssh-host
         (host "aur.archlinux.org")
         (options `((identity-file . "~/.ssh/aur")
                    (user . "aur"))))))))
    (feature-virtualization)
    (feature-qutebrowser)
    (feature-firefox
     #:add-keybindings? #f)
    (feature-kdeconnect)
    (feature-bitwarden-cli
     #:email "frewacom@gmail.com"
     #:lock-timeout 120))
   %engstrand-emacs-base-features
   (modify-features %engstrand-base-features
                    (delete 'ssh))))
