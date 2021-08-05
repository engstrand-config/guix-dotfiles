(define-module (engstrand systems)
               #:use-module (rde features)
               #:use-module (rde features system)
               #:use-module (rde features keyboard)
               #:use-module (gnu system keyboard))

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
