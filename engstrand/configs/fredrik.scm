(define-module (engstrand configs fredrik)
  #:use-module (rde features)
  #:use-module (rde features ssh)
  #:use-module (rde features base)
  #:use-module (rde features gnupg)
  #:use-module (gnu services)
  #:use-module (gnu services databases)
  #:use-module (gnu home-services ssh) ;; rde home-service
  #:use-module (farg source pywal)
  #:use-module (engstrand utils)
  #:use-module (engstrand configs)
  #:use-module (engstrand wallpapers) ;; get-wallpaper-path
  #:use-module (engstrand features base)
  #:use-module (engstrand features xorg)
  #:use-module (engstrand features sync)
  #:use-module (engstrand features utils)
  #:use-module (engstrand features state)
  #:use-module (engstrand features emacs)
  #:use-module (engstrand features publish)
  #:use-module (engstrand features theming)
  #:use-module (engstrand features browsers)
  #:use-module (engstrand features documents)
  #:use-module (engstrand features virtualization)
  #:use-module (engstrand features wayland))

(define-public %user-theme
  (farg:generator-pywal (get-wallpaper-path "f1/9w30A0D.jpg")
                        #:light? #f
                        #:alpha 0.96))

(define-public %user-features
  (append
   (list
    (feature-user-info
     #:user-name "fredrik"
     #:full-name "Fredrik Engstrand"
     #:email "fredrik@engstrand.nu")
    (feature-gnupg
     #:gpg-primary-key "C9BEB8A04458FDDF12681B39029D8EB77E18D68C"
     #:pinentry-flavor 'gtk2)
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
    (feature-docker)
    (feature-qutebrowser
     #:default-browser? #t)
    (feature-firefox)
    (feature-zathura)
    (feature-sioyek
     #:default-reader? #t)
    (feature-kdeconnect))
   %engstrand-emacs-base-features
   (modify-features %engstrand-base-features
                    (delete 'zathura)
                    (delete 'ssh))))
