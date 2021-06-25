;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules 
	(gnu) 
	(engstrand packages)
	(nongnu packages linux)
	(nongnu system linux-initrd))

(use-service-modules desktop networking ssh xorg)

(operating-system
  (kernel linux)
  (initrd microcode-initrd)
  (firmware (list linux-firmware))
  (locale "en_US.utf8")
  (timezone "Europe/Stockholm")
  (keyboard-layout
	(keyboard-layout "us,se" #:options '("grp:alt_shift_toggle" "grp_led:caps" "caps:escape")))
  (host-name "fredrik-laptop")
  (users (cons* (user-account
                  (name "fredrik")
                  (comment "Fredrik Engstrand")
                  (group "users")
                  (home-directory "/home/fredrik")
                  (supplementary-groups
                    '("wheel" "netdev" "audio" "video")))
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
            (set-xorg-configuration
              (xorg-configuration
		(extra-config '("Section \"Device\"
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
