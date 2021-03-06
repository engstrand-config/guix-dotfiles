(define-module (engstrand configs johan)
  #:use-module (gnu services)
  #:use-module (rde features base)
  #:use-module (rde features gnupg)
  #:use-module (rde features bittorrent)
  #:use-module (engstrand features sync)
  #:use-module (engstrand features browsers)
  #:use-module (engstrand features emacs)
  #:use-module (engstrand features virtualization)
  #:use-module (engstrand features wayland)
  #:use-module (dwl-guile utils)
  #:use-module (dwl-guile home-service)
  #:use-module (engstrand utils)
  #:use-module (engstrand features utils)
  #:use-module (engstrand configs))

;; TODO: Add custom packages as feature:
;;       "youtube-dl"

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
    (feature-virtualization)
    (feature-kdeconnect)
    (feature-transmission)
    (feature-piper)
    (feature-qutebrowser)
    (feature-firefox
     #:default-browser? #t)
    (feature-custom-services
     #:home-services
     (list
      (simple-service
       'change-dwl-guile-borderpx
       home-dwl-guile-service-type
       (modify-dwl-guile-config
        (config =>
                (dwl-config
                 (inherit config)
                 (border-px 2)))))))
    (feature-wayland-wbg
     #:path (string-append (getenv "HOME")
                           "/engstrand-config/wallpapers/firewatch/e8u3zvnatbm51.jpg")))
   %engstrand-emacs-base-features
   %engstrand-base-features))
