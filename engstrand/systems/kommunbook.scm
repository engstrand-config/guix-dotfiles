(define-module (engstrand systems kommunbook)
  #:use-module (engstrand utils)
  #:use-module (engstrand systems)
  #:use-module (engstrand features laptop)
  #:use-module (engstrand features radio)
  #:use-module (engstrand features dwl-guile)
  #:use-module (dwl-guile home-service)
  #:use-module (rde features base)
  #:use-module (rde features system)
  #:use-module (guix gexp)
  #:use-module (gnu system file-systems)
  #:use-module (gnu services)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu system mapped-devices)
  #:use-module (nongnu packages linux))

(define-public %system-features
  (append
   (list
    (feature-kernel
     #:kernel linux
     #:firmware (list linux-firmware)
     #:kernel-arguments %engstrand-kernel-arguments)
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
        (uuid "84ed6452-d8c9-4649-bfd4-c3171a7985cf"
              'btrfs))
       (type "btrfs")))
     #:swap-devices
     (list
      (swap-space
       (target (uuid "4ea38741-230c-47c4-97d6-132f2002d9fb")))))
    (feature-radio
     #:rtl-sdr? #t)
    (feature-dwl-guile-custom-config
     #:config
     `((setq gaps-oh 0
             gaps-ov 0
             gaps-ih 0
             gaps-iv 0
             border-px 3))))
   %engstrand-laptop-base-features))
