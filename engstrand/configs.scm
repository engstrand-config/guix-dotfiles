(define-module (engstrand configs)
               #:use-module (guix gexp)
               #:use-module (gnu packages fonts)
               #:use-module (rde features)
               #:use-module (rde features xdg)
               #:use-module (rde features ssh)
               #:use-module (rde features base)
               #:use-module (rde features linux)
               #:use-module (rde features fontutils)
               #:use-module (rde features version-control)
               #:use-module (dwl-guile patches)
               #:use-module (dwl-guile home-service)
               #:use-module (engstrand utils)
               #:use-module (engstrand systems)
               #:use-module (engstrand features nix)
               #:use-module (engstrand features base)
               #:use-module (engstrand features utils)
               #:use-module (engstrand features video)
               #:use-module (engstrand features state)
               #:use-module (engstrand features shells)
               #:use-module (engstrand features wayland)
               #:use-module (engstrand features browsers)
               #:use-module (engstrand features documents)
               #:export (
                         %engstrand-base-system-packages
                         %engstrand-base-home-packages
                         %engstrand-base-features))

; This module is responsible for creating the rde config.
; It will define all the different base system services.
;
; Operating system configuration should be done in engstrand/systems.scm,
; and computer specific settings in each corresponding file in engstrand/systems/.

; TODO: Add feature for setting custom groups (preferrably directly in features).
;       This is required by certain services, e.g. virtualization.

; TODO: Move these package lists into separate files (like manifests?)
; TODO: Move neovim to feature?
(define %engstrand-base-system-packages
  (pkgs '("git" "nss-certs")))

(define %engstrand-base-home-packages
  ;"engstrand-utils"
  (pkgs '("curl" "htop" "neovim"  "ncurses")))

; Dynamically create a configuration that can be reproduced
; without having the correct environment variables set.
; This is required for some commands to work, e.g. guix pull.
(define (make-entrypoint)
  (scheme-file "entrypoint.scm"
               #~(begin
                   (use-modules (engstrand reconfigure))
                   (make-config #:user #$(getenv "RDE_USER")
                                #:system #$(gethostname)))))

(define %engstrand-base-features
  (list
    (feature-base-services)
    (feature-desktop-services)
    (feature-switch-to-tty-on-boot)
    ; TODO: Move to systems/*.scm?
    (feature-hidpi
      #:console-font (file-append font-terminus "/share/consolefonts/ter-120b"))
    (feature-git
      #:sign-commits? #t)
    (feature-fonts)
    (feature-pipewire)
    (feature-backlight)
    (feature-zsh)
    (feature-ssh)
    (feature-xdg
      #:xdg-user-directories-configuration
      (home-xdg-user-directories-configuration
        (download "$HOME/downloads")
        (documents "$HOME/documents")
        (pictures "$HOME/images")
        (music "$HOME/music")
        (videos "$HOME/videos")
        (publicshare "$HOME")
        (templates "$HOME")
        (desktop "$HOME")))
    (feature-base-packages
      #:system-packages %engstrand-base-system-packages
      #:home-packages %engstrand-base-home-packages)
    (feature-state-git
      #:repos
      `(("engstrand-config/utils" .
         "git@github.com:engstrand-config/utils.git")
        ("engstrand-config/home-dwl-service" .
         "git@github.com:engstrand-config/home-dwl-service.git")
        ("engstrand-config/farg" .
         "git@github.com:engstrand-config/farg.git")))
    (feature-dotfiles
      #:dotfiles
      `(("aliasrc" ,(local-file "files/aliasrc"))
        ("inputrc" ,(local-file "files/inputrc"))
        ("nix-channels" ,(local-file "files/nix-channels"))
        ("config/guix/channels.scm" ,(local-file "channels.scm"))
        ("config/guix/config.scm" ,(make-entrypoint))
        ("config/dunst/dunstrc" ,(local-file "files/config/dunst/dunstrc"))
        ("config/nvim/init.vim" ,(local-file "files/config/nvim/init.vim"))
        ("config/nvim/autoload/plug.vim" ,(local-file "files/config/nvim/autoload/plug.vim"))
        ("config/picom/picom.conf" ,(local-file "files/config/picom/picom.conf"))))
    (feature-nix)
    (feature-mpv)
    (feature-obs)
    (feature-imv)
    (feature-zathura)
    (feature-firefox)
    (feature-wayland-bemenu)
    (feature-wayland-foot)
    (feature-wayland-mako)
    (feature-wayland-wlsunset)
    (feature-wayland-screenshot)
    (feature-wayland-dwl-guile
      #:dwl-guile-configuration
      (home-dwl-guile-configuration
        (patches %engstrand-dwl-guile-patches)
        (config %engstrand-dwl-guile-config)))))
