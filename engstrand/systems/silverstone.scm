(define-module (engstrand systems silverstone)
  #:use-module (engstrand utils)
  #:use-module (engstrand systems)
  #:use-module (engstrand features display)
  #:use-module (engstrand features bluetooth)
  #:use-module (rde features system)
  #:use-module (guix gexp)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (dwl-guile home-service)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system mapped-devices))

(define %mapped-devices
  (list
   (mapped-device
    (source
     (uuid "96cfdf5b-2b0b-4e10-adc0-d94cff2bc0b8"))
    (target "cryptroot")
    (type luks-device-mapping))))

(define-public %system-features
  (list
   (feature-host-info
    #:host-name "silverstone"
    #:timezone %engstrand-timezone
    #:locale %engstrand-locale)
   (feature-bootloader
    #:bootloader-configuration
    (bootloader-configuration
     (bootloader grub-efi-bootloader)
     (targets '("/boot/efi"))
     (keyboard-layout %engstrand-keyboard-layout)))
   (feature-bluetooth)
   (feature-file-systems
    #:mapped-devices %mapped-devices
    #:file-systems
    (list
     (file-system
      (mount-point "/boot/efi")
      (device (uuid "4CCA-4361" 'fat32))
      (type "vfat"))
     (file-system
      (mount-point "/")
      (device "/dev/mapper/cryptroot")
      (type "ext4")
      (dependencies %mapped-devices))))
   (feature-dwl-guile-monitor-config
    #:rules
    `((set-monitor-rules
       '((name "DP-2")
         (width 2560)
         (height 1440)
         (refresh-rate 240)
         (adaptive-sync? #t)))))))
