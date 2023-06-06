(define-module (engstrand configs johan)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (rde features base)
  #:use-module (rde features gnupg)
  #:use-module (rde features web-browsers)
  #:use-module (farg source pywal)
  #:use-module (engstrand utils)
  #:use-module (engstrand configs)
  #:use-module (engstrand themes)
  #:use-module (engstrand wallpapers) ;; get-wallpaper-path
  #:use-module (engstrand features daw)
  #:use-module (engstrand features documents)
  #:use-module (engstrand features sync)
  #:use-module (engstrand features emacs)
  #:use-module (engstrand features utils)
  #:use-module (engstrand features wayland)
  #:use-module (engstrand features theming)
  #:use-module (engstrand features web-browsers)
  #:use-module (engstrand features virtualization))

(define-public %user-theme
  (engstrand-theme-dark
   #:wallpaper
   (get-wallpaper-path "art/the-cleveland-museum-of-art-3_m6yeqmbv8-unsplash.jpg")))
(define-public %user-features
  (append
   (list
    (feature-user-info
     #:user-name "johan"
     #:full-name "Johan Engstrand"
     #:email %engstrand-email-primary)
    (feature-gnupg
     #:gpg-primary-key "DFC6C6B70EF5F7CB75EE97E6DF3088DDBCAD566D"
     #:pinentry-flavor 'bemenu)
    (feature-virtualization)
    (feature-kdeconnect)
    (feature-piper)
    (feature-sioyek)
    (feature-ungoogled-chromium)
    (feature-firefox
     #:default-browser? #t))
   %engstrand-emacs-base-features
   %engstrand-base-features))
