(define-module (engstrand systems pavilion)
  #:use-module (engstrand utils)
  #:use-module (engstrand systems)
  #:use-module (engstrand features laptop)
  #:use-module (rde features system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system mapped-devices))

(define-public %system-swap
  (swap-space
   (target (uuid "62f47965-ad3e-40a9-bb5e-46e4387fa449"))))

(define-public %system-features
  (append
   (list
    (feature-host-info
     #:host-name "pavilion"
     #:timezone %engstrand-timezone
     #:locale %engstrand-locale)
    (feature-bootloader)
    (feature-file-systems
     #:file-systems
     (list
      (file-system
       (mount-point "/boot/efi")
       (device (uuid "1ADC-28E8" 'fat32))
       (type "vfat"))
      (file-system
       (mount-point "/")
       (device
        (uuid "6c3ee1c8-6ee6-4142-b2bf-a370854f63e7"
              'ext4))
       (type "ext4")))))
   %engstrand-laptop-base-features))
