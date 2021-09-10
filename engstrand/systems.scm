(define-module (engstrand systems)
               #:use-module (rde features)
               #:use-module (rde features base)
               #:use-module (rde features system)
               #:use-module (rde features keyboard)
               #:use-module (gnu system)
               #:use-module (gnu system keyboard)
               #:use-module (gnu system file-systems)
               #:use-module (gnu bootloader)
               #:use-module (gnu bootloader grub)
               #:use-module (nongnu packages linux)
               #:use-module (nongnu system linux-initrd)
               #:export (
                         %engstrand-timezone
                         %engstrand-locale
                         %engstrand-kernel-arguments
                         %engstrand-keyboard-layout
                         %engstrand-initial-os
                         %engstrand-system-base-features))

; This module is responsible for configuring an operating system,
; i.e. kernel, microcode, hostname, keyboard layout, etc.
;
; Base packages, services and other features should be defined in
; engstrand/configs, or in one of the custom configs at engstrand/configs/.

(define-public %engstrand-timezone "Europe/Stockholm")
(define-public %engstrand-locale "en_US.utf8")

(define-public %engstrand-kernel-arguments
               (list "modprobe.blacklist=pcspkr,snd_pcsp"
                     "quiet"))

(define-public %engstrand-keyboard-layout
               (keyboard-layout "us,se"
                                #:options
                                '("grp:alt_shift_toggle" "grp_led:caps" "caps:escape")))

(define-public %engstrand-initial-os
               (operating-system
                 (host-name "engstrand")
                 (locale  %engstrand-locale)
                 (timezone  %engstrand-timezone)
                 (kernel linux)
                 (firmware (list linux-firmware))
                 (initrd microcode-initrd)
                 (kernel-arguments %engstrand-kernel-arguments)
                 (keyboard-layout %engstrand-keyboard-layout)
                 (bootloader (bootloader-configuration
                               (bootloader grub-efi-bootloader)
                               (target "/boot/efi")))
                 (services '())
                 (file-systems %base-file-systems)
                 (issue "This is the GNU/Engstrand system. Welcome.\n")))

(define-public %engstrand-system-base-features
               (list
                 (feature-custom-services
                   #:home-services
                   (list
                     ;; TODO: Remove it once upstreamed.
                     ((@ (gnu services) simple-service)
                      'make-guix-aware-of-guix-home-subcomand
                      (@ (gnu home-services) home-environment-variables-service-type)
                      '(("GUILE_LOAD_PATH" .
                         "$XDG_CONFIG_HOME/guix/current/share/guile/site/3.0\
                         :$GUILE_LOAD_PATH")
                        ("GUILE_LOAD_COMPILED_PATH" .
                         "$XDG_CONFIG_HOME/guix/current/lib/guile/3.0/site-ccache\
                         :$GUILE_LOAD_COMPILED_PATH")))))
                 (feature-keyboard
                   #:keyboard-layout %engstrand-keyboard-layout)))
