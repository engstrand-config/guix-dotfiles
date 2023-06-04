(define-module (engstrand configs johan)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (rde features base)
  #:use-module (rde features gnupg)
  #:use-module (rde features bittorrent)
  #:use-module (rde features mail)
  #:use-module (farg source pywal)
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
  #:use-module (engstrand features virtualization))

(define-public %user-theme
  (farg:generator-pywal (get-wallpaper-path "nature/jack-b-8Wqm1W59Baw-unsplash.jpg")
                        #:light? #f
                        #:alpha 0.96))

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
    (feature-transmission
     #:download-dir "~/downloads")
    (feature-piper)
    (feature-ardour)
    (feature-calf-plugins)
    ;; (feature-sfz)
    (feature-mail-settings
     #:mail-accounts (list (mail-account (id 'personal) (fqda email-engstrand-primary) (type 'generic))))
    (feature-emacs-message)
    (feature-notmuch)
    (feature-msmtp
     #:msmtp-provider-settings
     `((generic . ((host . "eagle.mxlogin.com")
                   (port . 587)
                   (tls_starttls . on)))))
    (feature-isync #:isync-verbose #t)
    (feature-l2md)
    (feature-qutebrowser)
    (feature-firefox
     #:default-browser? #t))
   %engstrand-emacs-base-features
   %engstrand-base-features))
