(define-module (engstrand systems)
               #:use-module (rde features)
               #:use-module (rde features system)
               #:use-module (rde features keyboard)
               #:use-module (gnu system keyboard))

; TODO: Will this require a new feature?
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

; TODO: Add feature for setting custom groups (preferrably directly in features).
;       This is required by certain services, e.g. virtualization.

; TODO: Add feature for setting microcode:
;       (initrd microcode-initrd)

; TODO: Set issue in initial-os:
;       (issue "This is the GNU/Engstrand system. Welcome.\n")))

; TODO: Add base packages:
;       "git" "curl" "htop" "neovim" "nss-certs" "openssh" "pulseaudio" "engstrand-utils"
;       "ncurses" "gnupg" "pinentry" "zsh"

; TODO: Add XDG feature
;       (service home-xdg-user-directories-service-type
;                (home-xdg-user-directories-configuration
;                  (download (abspath home "downloads"))
;                  (documents (abspath home "documents"))
;                  (pictures (abspath home "images"))
;                  (music (abspath home "music"))
;                  (videos (abspath home "videos"))
;                  (publicshare home)
;                  (templates home)
;                  (desktop home)))

; TODO: Add ssh service:
;       (service home-ssh-service-type)

; (define (abspath homedir path) (string-append homedir "/" path))

(define-public %engstrand-timezone "Europe/Stockholm")
(define-public %engstrand-locale "en_US.utf8")

(define-public %engstrand-kernel-arguments
               (list "modprobe.blacklist=pcspkr,snd_pcsp"
                     "quiet"))

(define-public %engstrand-keyboard-layout
               (keyboard-layout "us,se"
                                #:options
                                '("grp:alt_shift_toggle" "grp_led:caps" "caps:escape")))

(define-public %engstrand-base-features
               (list
                 (feature-host-info
                   #:host-name)
                 (feature-keyboard
                   #:keyboard-layout %engstrand-keyboard-layout)
                 (feature-boot-loader)))
