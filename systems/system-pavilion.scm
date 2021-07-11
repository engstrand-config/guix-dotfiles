(define-module (systems system-pavilion)
               #:use-module (systems system-base)
               #:use-module (users user-johan)
               #:use-module (gnu))

(operating-system
  (inherit (base-operating-system
             #:user johan
             #:laptop? #t
             #:host-name "pavilion"
             #:nix? #t))
  (swap-devices
    (list (uuid "62f47965-ad3e-40a9-bb5e-46e4387fa449")))
  (file-systems
    (cons* (file-system
             (mount-point "/boot/efi")
             (device (uuid "1ADC-28E8" 'fat32))
             (type "vfat"))
           (file-system
             (mount-point "/")
             (device
               (uuid "6c3ee1c8-6ee6-4142-b2bf-a370854f63e7"
                     'ext4))
             (type "ext4"))
           %base-file-systems)))
