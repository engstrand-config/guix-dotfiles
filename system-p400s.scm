(define %user "johan")
(define %full-name "Johan Engstrand")

(use-modules
	(gnu)
        (gnu services pm)
	(engstrand packages)
	(nongnu packages linux)
	(nongnu system linux-initrd))

(use-service-modules nix)
(use-package-modules package-management)

(use-service-modules desktop networking ssh xorg)

(operating-system
  (kernel linux)
  (initrd microcode-initrd)
  (firmware (list linux-firmware))
  (locale "en_US.utf8")
  (timezone "Europe/Stockholm")
  (keyboard-layout
	(keyboard-layout "us,se" #:options '("grp:alt_shift_toggle" "grp_led:caps" "caps:escape")))
  (host-name "p400s")
  (users (cons* (user-account
                  (name %user)
                  (comment %full-name)
                  (group "users")
                  (home-directory (string-append "/home/" %user))
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
	  (specification->package "engstrand-utils")
          (specification->package "nix"))
	  ;(specification->package "engstrand-dmenu")
      %base-packages))
  (services
    (append
      (list (service nix-service-type)
            (set-xorg-configuration
              (xorg-configuration
                (keyboard-layout keyboard-layout))))
      %desktop-services))
  (bootloader
    (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (target "/boot/efi")
      (keyboard-layout keyboard-layout)))
  (swap-devices
    (list (uuid "2e1cafae-8f43-4938-be21-fc525250f915")))
  (file-systems
    (cons* (file-system
             (mount-point "/boot/efi")
             (device (uuid "8396-F2E9" 'fat32))
             (type "vfat"))
           (file-system
             (mount-point "/")
             (device
               (uuid "3d72e2f8-6474-4b99-8087-48094ed37f2b"
                     'ext4))
             (type "ext4"))
           %base-file-systems))
  (issue "This is the GNU/Engstrand system. Welcome."))
