(define-module (engstrand systems kommunbook)
  #:use-module (engstrand utils)
  #:use-module (engstrand systems)
  #:use-module (engstrand features laptop)
  #:use-module (engstrand features display)
  #:use-module (engstrand features bluetooth)
  #:use-module (rde features system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu system mapped-devices))

(define-public %system-swap
  (swap-space
   (target (uuid "2055b78a-6584-490e-a51f-9ddc1195fc94"))))

(define-public %system-features
  (list
   (feature-host-info
    #:host-name "kommunbook"
    #:timezone %engstrand-timezone
    #:locale %engstrand-locale)
   (feature-bootloader
    #:bootloader-configuration
    (bootloader-configuration
     (bootloader grub-bootloader)
     (targets (list "/dev/sda"))))
   (feature-file-systems
    #:file-systems
    (list
     (file-system
      (mount-point "/")
      (device
       (uuid "dd627ff0-9b52-4396-b67c-ddc81c5c6d38"
             'ext4))
      (type "ext4"))))
   (feature-laptop)
   (feature-laptop-monitor-brightness)
   (feature-bluetooth)
   (feature-tlp)))
