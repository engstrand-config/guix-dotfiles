(define-module (engstrand systems)
               #:use-module (rde features)
               #:use-module (rde features system)
               #:use-module (rde features keyboard)
               #:use-module (gnu system keyboard))

; This module is responsible for configuring an operating system,
; i.e. kernel, microcode, hostname, keyboard layout, etc.
;
; Base packages, services and other features should be defined in
; engstrand/configs, or in one of the custom configs at engstrand/configs/.

; TODO: Add feature for setting microcode:
;       (initrd microcode-initrd)

; TODO: Set issue in initial-os:
;       (issue "This is the GNU/Engstrand system. Welcome.\n")))

(define-public %engstrand-timezone "Europe/Stockholm")
(define-public %engstrand-locale "en_US.utf8")

(define-public %engstrand-kernel-arguments
               (list "modprobe.blacklist=pcspkr,snd_pcsp"
                     "quiet"))

(define-public %engstrand-keyboard-layout
               (keyboard-layout "us,se"
                                #:options
                                '("grp:alt_shift_toggle" "grp_led:caps" "caps:escape")))

(define-public %engstrand-base-system
               (list
                 (feature-keyboard
                   #:keyboard-layout %engstrand-keyboard-layout)
                 (feature-bootloader)))
