;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules (gnu) (nongnu packages linux))
(use-service-modules desktop networking ssh xorg)

(operating-system
  (kernel linux)
  (firmware (list linux-firmware))
  (locale "en_US.utf8")
  (timezone "Europe/Stockholm")
  (keyboard-layout (keyboard-layout "us"))
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
      (list (specification->package "nss-certs"))
      %base-packages))
  (services
    (append
      (list (service gnome-desktop-service-type)
            (set-xorg-configuration
              (xorg-configuration
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
