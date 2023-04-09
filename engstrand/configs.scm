(define-module (engstrand configs)
  #:use-module (guix gexp)
  #:use-module (gnu packages fonts)
  #:use-module (rde features)
  #:use-module (rde features xdg)
  #:use-module (rde features ssh)
  #:use-module (rde features base)
  #:use-module (rde features shells)
  #:use-module (rde features linux)
  #:use-module (rde features shellutils)
  #:use-module (rde features fontutils)
  #:use-module (rde features image-viewers)
  #:use-module (rde features video)
  #:use-module (rde features version-control)
  #:use-module (dwl-guile patches)
  #:use-module (dwl-guile home-service)
  #:use-module (dtao-guile home-service)
  #:use-module (farg provider)
  #:use-module (farg home-service)
  #:use-module (engstrand utils)
  #:use-module (engstrand systems)
  #:use-module (engstrand features nix)
  #:use-module (engstrand features base)
  #:use-module (engstrand features audio)
  #:use-module (engstrand features utils)
  #:use-module (engstrand features video)
  #:use-module (engstrand features state)
  #:use-module (engstrand features shells)
  #:use-module (engstrand features neovim)
  #:use-module (engstrand features theming)
  #:use-module (engstrand features wayland)
  #:use-module (engstrand features dwl-guile)
  #:use-module (engstrand features documents)
  #:use-module (engstrand features statusbar)
  #:use-module (engstrand features messaging)
  #:use-module (engstrand features networking)
  #:use-module (engstrand features version-control)
  #:export (
            %engstrand-base-system-packages
            %engstrand-base-home-packages
            %engstrand-base-features))

;; This module is responsible for creating the rde config.
;; It will define all the different base system services.
;;
;; Operating system configuration should be done in engstrand/systems.scm,
;; and computer specific settings in each corresponding file in engstrand/systems/.
(define %engstrand-base-system-packages
  (pkgs '("git" "nss-certs")))

;; Move some of the packages to separate features?
(define %engstrand-base-home-packages
  (pkgs '("curl" "htop" "ncurses"
          "hicolor-icon-theme" "adwaita-icon-theme" "gnome-themes-extra")))

;; Dynamically create a configuration that can be reproduced
;; without having the correct environment variables set.
;; This is required for some commands to work, e.g. guix pull.
(define (make-entrypoint)
  (scheme-file "entrypoint.scm"
               #~(begin
                   (use-modules (engstrand reconfigure))
                   (make-config #:user #$(getenv "RDE_USER")
                                #:system #$(gethostname)))))

(define %engstrand-base-features
  (list
   (feature-base-services
    #:guix-substitute-urls (list "https://substitutes.nonguix.org")
    #:guix-authorized-keys (list (local-file "files/nonguix-signing-key.pub")))
   (feature-desktop-services)
   (feature-networking)
   (feature-switch-to-tty-on-boot)
   ;; TODO: Move to systems/*.scm?
   (feature-hidpi
    #:console-font (file-append font-terminus "/share/consolefonts/ter-120b"))
   (feature-git
    #:sign-commits? #t)
   (feature-git-colorscheme)
   (feature-fonts
    #:font-packages (list font-jetbrains-mono font-iosevka-aile)
    #:font-monospace (font "JetBrains Mono" #:size 13)
    #:font-sans (font "Iosevka Aile" #:size 13)
    #:font-serif (font "Iosevka Aile" #:size 13))
   (feature-pipewire)
   (feature-pulseaudio-control)
   (feature-backlight)
   (feature-bash)
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
   ;; (feature-state-git
   ;;   #:repos
   ;;   `(("engstrand-config/utils" .
   ;;      "git@github.com:engstrand-config/utils.git")
   ;;     ("engstrand-config/home-dwl-service" .
   ;;      "git@github.com:engstrand-config/home-dwl-service.git")
   ;;     ("engstrand-config/farg" .
   ;;      "git@github.com:engstrand-config/farg.git")))
   (feature-dotfiles
    #:dotfiles
    `((".aliasrc" ,(local-file "files/aliasrc"))
      (".inputrc" ,(local-file "files/inputrc"))
      (".nix-channels" ,(local-file "files/nix-channels"))
      (".config/guix/channels.scm" ,(local-file "channels.scm"))
      (".config/guix/config.scm" ,(make-entrypoint))
      (".config/dunst/dunstrc" ,(local-file "files/config/dunst/dunstrc"))
      (".config/nvim/init.vim" ,(local-file "files/config/nvim/init.vim"))
      (".config/nvim/autoload/plug.vim" ,(local-file "files/config/nvim/autoload/plug.vim"))
      (".config/picom/picom.conf" ,(local-file "files/config/picom/picom.conf"))))
   (feature-farg)
   (feature-nix)
   (feature-mpv)
   (feature-youtube-dl)
   (feature-obs)
   (feature-imv)
   (feature-neovim)
   (feature-zathura
    #:default-reader? #t)
   (feature-signal)
   (feature-dwl-guile)
   (feature-wayland-swaybg)
   (feature-wayland-bemenu)
   (feature-wayland-bemenu-power)
   (feature-wayland-foot)
   (feature-wayland-mako)
   (feature-wayland-wlsunset)
   (feature-wayland-screenshot)
   (feature-wayland-swaylock)
   (feature-statusbar-dtao-guile)))
