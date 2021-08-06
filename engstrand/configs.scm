(define-module (engstrand configs)
               #:use-module (guix gexp)
               #:use-module (rde features)
               #:use-module (rde features xdg)
               #:use-module (rde features ssh)
               #:use-module (rde features base)
               #:use-module (rde features linux)
               #:use-module (rde features fontutils)
               #:use-module (dwl-guile patches)
               #:use-module (dwl-guile home-service)
               #:use-module (engstrand utils)
               #:use-module (engstrand systems)
               #:use-module (engstrand features state)
               #:use-module (engstrand features shells)
               #:use-module (engstrand features wayland)
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

; TODO: Do we need this?
;       rde seems to have something like this already built in,
;       see features/linux.scm. Test it before adding the rules below.
; (define %backlight-udev-rule
;   (udev-rule
;     "90-backlight.rules"
;     (string-append "ACTION==\"add\", "
;                    "SUBSYSTEM==\"backlight\", "
;                    "RUN+=\"/run/current-system/profile/bin/chgrp video /sys/class/backlight/%k/brightness\"\n"
;                    "ACTION==\"add\", "
;                    "SUBSYSTEM==\"backlight\", "
;                    "RUN+=\"/run/current-system/profile/bin/chmod g+w /sys/class/backlight/%k/brightness\"\n"
;                    "ACTION==\"add\", "
;                    "SUBSYSTEM==\"leds\", "
;                    "RUN+=\"/run/current-system/profile/bin/chgrp input /sys/class/leds/%k/brightness\"\n"
;                    "ACTION==\"add\", "
;                    "SUBSYSTEM==\"leds\", "
;                    "RUN+=\"/run/current-system/profile/bin/chmod g+w /sys/class/leds/%k/brightness\"")))

; TODO: Move these package lists into separate files (like manifests?)
; TODO: Move neovim to feature?
(define %engstrand-base-system-packages
               (pkgs '("git" "nss-certs")))

(define %engstrand-base-home-packages
               ;"engstrand-utils"
               (pkgs '("curl" "htop" "neovim"  "ncurses")))

(define %engstrand-base-features
               (list
                 ; TODO: Pass in udev rules to base-services "udev-rules"?
                 (feature-base-services)
                 (feature-desktop-services)
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
                     ("engstrand-config/guix-channel" .
                      "git@github.com:engstrand-config/guix-channel.git")
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
                     ("config/dunst/dunstrc" ,(local-file "files/config/dunst/dunstrc"))
                     ("config/nvim/init.vim" ,(local-file "files/config/nvim/init.vim"))
                     ("config/nvim/autoload/plug.vim" ,(local-file "files/config/nvim/autoload/plug.vim"))
                     ("config/picom/picom.conf" ,(local-file "files/config/picom/picom.conf"))))))
