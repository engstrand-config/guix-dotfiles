(define-module (tuxedo)
               #:use-module (base-system)
               ;; The general laptop module should be added, name pending
               #:use-module (common-laptop)
               #:use-module (gnu))

(define %xorg-amdgpu-config
  "Section \"Device\"
        Identifier  \"AMD\"
        Driver      \"amdgpu\"
        Option      \"TearFree\" \"true\"
        Option      \"Backlight\" \"amdgpu_bl0\"
    EndSection")

(operating-system
  (inherit base-operating-system)
  (host-name "tuxedo")
  (services
    (append
      (list (set-xorg-configuration
              (xorg-configuration
		(extra-config (list %xorg-amdgpu-config)))))))
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
