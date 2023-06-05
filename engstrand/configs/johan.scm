(define-module (engstrand configs johan)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (rde features base)
  #:use-module (rde features gnupg)
  #:use-module (rde features web-browsers)
  #:use-module (rde features mail)
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
  #:use-module (engstrand features browsers)
  #:use-module (engstrand features virtualization))

(define-public %user-theme engstrand-theme-dark)

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
    (feature-piper)
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
    (feature-sioyek)
    (feature-ungoogled-chromium)
    (feature-firefox
     #:default-browser? #t))
   %engstrand-emacs-base-features
   %engstrand-base-features))
