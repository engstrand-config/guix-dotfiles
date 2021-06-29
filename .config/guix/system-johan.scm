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

; (define %pavilion-graphics
; "
; Section \"Device\"
;         Identifier      \"Device0\"
;         Driver          \"nvidia\"
;         VendorName      \"NVIDIA Corporation\"
;         BusID           \"PCI:1:0:0\"
; EndSection
; Section \"Device\"
;   Identifier  \"Intel Graphics\"
;   Driver      \"intel\"
;   Option      \"AccelMethod\" \"sna\"
;   Option      \"TearFree\" \"true\"
; EndSection
; ")

(use-modules
	(gnu)
        (gnu services pm)
	(engstrand packages)
	(nongnu packages linux)
	(nongnu system linux-initrd))

(use-service-modules nix)
(use-package-modules package-management)

(use-service-modules desktop networking ssh xorg)

; Allow users in the video group to modify backlight without using sudo
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

(operating-system
  (kernel linux)
  (kernel-arguments (list "modprobe.blacklist=pcspkr,snd_pcsp"))
  (initrd microcode-initrd)
  (firmware (list linux-firmware))
  (locale "en_US.utf8")
  (timezone "Europe/Stockholm")
  (keyboard-layout
	(keyboard-layout "us,se" #:options '("grp:alt_shift_toggle" "grp_led:caps" "caps:escape")))
  (host-name "pavilion")
  (users (cons* (user-account
                  (name "johan")
                  (comment "Johan Engstrand")
                  (group "users")
                  (home-directory "/home/johan")
                  (supplementary-groups
                    '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))

  (packages
    (append
      (list
	  (specification->package "nss-certs")
	  (specification->package "engstrand-dwm")
	  (specification->package "engstrand-st")
	  (specification->package "engstrand-dsblocks")
          (specification->package "nix"))
	  ;(specification->package "engstrand-dmenu")
      %base-packages))
  (services
    (append
      (list (service gnome-desktop-service-type)
            (service tlp-service-type
                (tlp-configuration
                    (cpu-scaling-governor-on-ac (list "performance"))
                    (sched-powersave-on-bat? #t)))
            (service nix-service-type)
	    (udev-rules-service 'backlight %backlight-udev-rule)
            (set-xorg-configuration
              (xorg-configuration
                (extra-config (list %xorg-libinput-config))
                (keyboard-layout keyboard-layout))))
      %desktop-services))
  (bootloader
    (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (target "/boot/efi")
      (keyboard-layout keyboard-layout)))
  (mapped-devices
    (list (mapped-device
            (source
              (uuid "19ae0e83-781a-4ad9-b5ac-cfa5d83e2512"))
            (target "cryptroot")
            (type luks-device-mapping))))
  (file-systems
    (cons* (file-system
             (mount-point "/boot/efi")
             (device (uuid "1ADC-28E8" 'fat32))
             (type "vfat"))
           (file-system
             (mount-point "/")
             (device "/dev/mapper/cryptroot")
             (type "ext4")
             (dependencies mapped-devices))
           %base-file-systems)))

