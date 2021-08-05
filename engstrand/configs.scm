(define-module (engstrand configs)
               #:use-module (rde features)
               #:use-module (rde features xdg)
               #:use-module (rde features ssh)
               #:use-module (rde features base)
               #:use-module (rde features linux)
               #:use-module (rde features shells)
               #:use-module (rde features fontutils)
               #:use-module (engstrand utils))

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
(define-public %engstrand-base-system-packages
               (pkgs '("git" "nss-certs")))

(define-public %engstrand-base-home-packages
               (pkgs '("curl" "htop" "neovim" "engstrand-utils" "ncurses")))

(define-public %engstrand-base-config
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
                   #:home-packages %engstrand-base-home-packages)))
