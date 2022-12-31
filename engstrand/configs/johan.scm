(define-module (engstrand configs johan)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (rde features base)
  #:use-module (rde features gnupg)
  #:use-module (rde features bittorrent)
  #:use-module (rde features mail)
  #:use-module (engstrand utils)
  #:use-module (engstrand configs)
  #:use-module (engstrand wallpapers) ;; get-wallpaper-path
  #:use-module (engstrand features daw)
  #:use-module (engstrand features sync)
  #:use-module (engstrand features emacs)
  #:use-module (engstrand features utils)
  #:use-module (engstrand features wayland)
  #:use-module (engstrand features theming)
  #:use-module (engstrand features browsers)
  #:use-module (engstrand features virtualization)
  #:use-module (farg config)
  #:use-module (dwl-guile utils)
  #:use-module (dwl-guile home-service))

(define-public %user-colorscheme
  (farg-config
   (inherit %engstrand-default-farg-config)
   (wallpaper (get-wallpaper-path "nature/jack-b-8Wqm1W59Baw-unsplash.jpg"))))

(define email-engstrand-primary "johan@engstrand.nu")

(define-public %user-features
  (append
   (list
    (feature-user-info
     #:user-name "johan"
     #:full-name "Johan Engstrand"
     #:email email-engstrand-primary)
    (feature-gnupg
     #:gpg-primary-key "DFC6C6B70EF5F7CB75EE97E6DF3088DDBCAD566D"
     #:pinentry-flavor 'gtk2)
    (feature-virtualization)
    (feature-kdeconnect)
    (feature-transmission)
    (feature-piper)
    (feature-ardour)
    (feature-calf-plugins)
    (feature-sfz)
    (feature-mail-settings
     #:mail-accounts (list (mail-account (id 'personal) (fqda email-engstrand-primary) (type 'generic))))
    (feature-emacs-message)
    (feature-isync)
    (feature-qutebrowser)
    (feature-firefox
     #:default-browser? #t)
    (feature-custom-services
     #:home-services
     (list
      (simple-service
       'change-dwl-guile-borderpx
       home-dwl-guile-extension
       #~((set 'smart-gaps? #f)
          (set 'smart-borders? #f)
          (set 'gaps-horizontal-outer 20)
          (set 'gaps-vertical-outer 20)
          (set 'border-px 2))))))
   %engstrand-emacs-base-features
   %engstrand-base-features))
