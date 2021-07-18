(define-module (systems system-base)
               #:use-module (srfi srfi-1) ;; for "remove"
               #:use-module (gnu)
               #:use-module (gnu services pm)
               #:use-module (gnu packages)
               #:use-module (gnu packages shells)
               #:use-module (gnu packages package-management)
               #:use-module (users user-base)
               #:use-module (engstrand packages)
               #:use-module (engstrand packages engstrand-dwm)
               #:use-module (engstrand packages engstrand-st)
               #:use-module (engstrand packages engstrand-dsblocks)
               #:use-module (engstrand packages engstrand-dmenu)
               #:use-module (engstrand packages engstrand-utils)
               #:use-module (nongnu packages linux)
               #:use-module (nongnu system linux-initrd)
               #:export (base-operating-system))

(define %xorg-libinput-config
  "
  Section \"InputClass\"
  Identifier \"Touchpads\"
  Driver \"libinput\"
  MatchDevicePath \"/dev/input/event*\"
  MatchIsTouchPad \"on\"
  Option \"DisableWhileTyping\" \"on\"
  Option \"NaturalScrolling\" \"true\"
  EndSection
  Section \"InputClass\"
  Identifier \"Keyboards\"
  Driver \"libinput\"
  MatchDevicePath \"/dev/input/event*\"
  MatchIsKeyboard \"on\"
  EndSection
  ")

(define %backlight-udev-rule
  (udev-rule
    "90-backlight.rules"
    (string-append "ACTION==\"add\", "
                   "SUBSYSTEM==\"backlight\", "
                   "RUN+=\"/run/current-system/profile/bin/chgrp video /sys/class/backlight/%k/brightness\"\n"
                   "ACTION==\"add\", "
                   "SUBSYSTEM==\"backlight\", "
                   "RUN+=\"/run/current-system/profile/bin/chmod g+w /sys/class/backlight/%k/brightness\"\n"
                   "ACTION==\"add\", "
                   "SUBSYSTEM==\"leds\", "
                   "RUN+=\"/run/current-system/profile/bin/chgrp input /sys/class/leds/%k/brightness\"\n"
                   "ACTION==\"add\", "
                   "SUBSYSTEM==\"leds\", "
                   "RUN+=\"/run/current-system/profile/bin/chmod g+w /sys/class/leds/%k/brightness\"")))

(use-package-modules package-management)
(use-service-modules desktop networking ssh xorg)
(use-service-modules nix)
(use-service-modules virtualization)

(define* (base-operating-system
           #:key
           (user '())
           (groups-extra '())
           (virt? #f)
           (laptop? #f)
           (kernel-modules '())
           (kernel-arguments '())
           (host-name "basehost")
           (packages '())
           (services '())
           (nix? #f)
           (xorg-extra '()))

         (if (not (system-user? user)) (throw 'invalid-user . (display "Invalid user argument, expected user record")))
         (operating-system
           (kernel linux)
           (kernel-loadable-modules (map (lambda (module) (module linux)) kernel-modules))
           (kernel-arguments (append (list "modprobe.blacklist=pcspkr,snd_pcsp")
                                     kernel-arguments))
           (initrd microcode-initrd)
           (firmware (list linux-firmware))

           (host-name host-name)

           (timezone "Europe/Stockholm")
           (locale "en_US.utf8")

           (keyboard-layout
             (keyboard-layout "us,se" #:options '("grp:alt_shift_toggle" "grp_led:caps" "caps:escape")))

           (users (cons* (user-account
                           (name (system-user-account user))
                           (comment (system-user-name user))
                           (shell (file-append zsh "/bin/zsh"))
                           (group "users")
                           (home-directory (string-append "/home/" (system-user-account user)))
                           (supplementary-groups
                             (append
                               (list "wheel" "netdev" "audio" "video")
                               groups-extra
                               (if virt? (list "libvirt" "kvm") '()))))
                         %base-user-accounts))

           (packages
             (append
               (list
                 (if nix? (specification->package "nix")))
               (map specification->package
                    '("git" "curl" "neovim"
                      "nss-certs"
                      "openssh"
                      "pulseaudio"
                      "engstrand-dwm" "engstrand-dmenu" "engstrand-dsblocks"
                      "engstrand-st"
                      "engstrand-utils"))
               packages
               %base-packages))

           (services
             (append
               ; virtualization services
               (if virt?
                   (list
                     (service virtlog-service-type)
                     (service libvirt-service-type
                              (libvirt-configuration (unix-sock-group "libvirt"))))
                   '())
               ; laptop services
               (if laptop?
                   (list
                     (udev-rules-service 'backlight %backlight-udev-rule)
                     (service tlp-service-type
                              (tlp-configuration
                                (cpu-scaling-governor-on-ac (list "performance"))
                                (sched-powersave-on-bat? #t))))
                   '())
               ; nix services
               (if nix?
                   (list
                     (service nix-service-type))
                   '())
               ; base services
               (list
                 (service slim-service-type
                          (slim-configuration
                            (auto-login? #f)
                            (default-user (system-user-account user))
                            (xorg-configuration
                              (xorg-configuration
                                (keyboard-layout keyboard-layout)
                                (extra-config (append (list %xorg-libinput-config) xorg-extra)))))))
               services
               (modify-services %desktop-services
                                (delete gdm-service-type))))

           (bootloader (bootloader-configuration
                         (bootloader grub-efi-bootloader)
                         (target "/boot/efi")
                         (keyboard-layout keyboard-layout)))

           ;; file-systems must be overwritten with system-specific settings
           (file-systems (cons*
                           (file-system
                             (mount-point "/tmp")
                             (device "none")
                             (type "tmpfs")
                             (check? #f))
                           %base-file-systems))

           (issue "This is the GNU/Engstrand system. Welcome.\n")))
