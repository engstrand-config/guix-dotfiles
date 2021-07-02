;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules
	(gnu)
        (gnu services pm)
	(engstrand packages)
	(nongnu packages linux)
	(nongnu system linux-initrd))

(use-service-modules desktop networking ssh xorg virtualization)

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
  (initrd microcode-initrd)
  (firmware (list linux-firmware))
  (locale "en_US.utf8")
  (timezone "Europe/Stockholm")
  (keyboard-layout
	(keyboard-layout "us,se" #:options '("grp:alt_shift_toggle" "grp_led:caps" "caps:escape")))
  (host-name "tuxedo")
  (users (cons* (user-account
                  (name "fredrik")
                  (comment "Fredrik Engstrand")
                  (group "users")
                  (home-directory "/home/fredrik")
                  (supplementary-groups
                    '("wheel" "netdev" "audio" "video" "libvirt" "kvm")))
                %base-user-accounts))
  (packages
    (append
      (list
	  (specification->package "nss-certs")
	  (specification->package "engstrand-dwm")
	  (specification->package "engstrand-st")
	  (specification->package "engstrand-dsblocks"))
	  ;(specification->package "engstrand-dmenu")
      %base-packages))
  (services
    (append
      (list (service gnome-desktop-service-type)
            (service virtlog-service-type)
            (service libvirt-service-type
                (libvirt-configuration (unix-sock-group "libvirt")))
            (service tlp-service-type
                (tlp-configuration
                    (cpu-scaling-governor-on-ac (list "performance"))
                    (sched-powersave-on-bat? #t)))
	    (udev-rules-service 'backlight %backlight-udev-rule)
            (set-xorg-configuration
              (xorg-configuration
		(extra-config
		  '("Section \"Device\"
                     	Identifier  \"AMD\"
                     	Driver      \"amdgpu\"
                     	Option      \"TearFree\" \"true\"
                     	Option      \"Backlight\" \"amdgpu_bl0\"
                     EndSection"))
                (keyboard-layout keyboard-layout))))
      %desktop-services))
  (bootloader
    (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (target "/boot/efi")
      (keyboard-layout keyboard-layout)))
  (swap-devices
    (list (uuid "40c98866-74b1-4e99-9c32-24d584fe0617")))
  (file-systems
    (cons* (file-system
             (mount-point "/boot/efi")
             (device (uuid "7E51-6BDB" 'fat32))
             (type "vfat"))
           (file-system
             (mount-point "/")
             (device
               (uuid "4484aa6c-d5ff-4964-b62d-c2572c701e66"
                     'ext4))
             (type "ext4"))
           %base-file-systems)))
