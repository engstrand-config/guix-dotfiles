(define-module (engstrand systems silverstone)
  #:use-module (engstrand utils)
  #:use-module (engstrand systems)
  #:use-module (nongnu packages linux)
  #:use-module (gnu packages linux)
  #:use-module (engstrand features bluetooth)
  #:use-module (engstrand features display)
  #:use-module (engstrand features daw)
  #:use-module (rde features base)
  #:use-module (rde features bluetooth)
  #:use-module (rde features system)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu services)
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
   (feature-kernel
    #:kernel linux ;; (corrupt-linux linux-libre #:configs '("CONFIG_MT7921E=m"))
    #:firmware (list linux-firmware)
    #:kernel-arguments %engstrand-kernel-arguments)
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
   (feature-bluetooth-quick-connect)
   (feature-ardour)
   (feature-calf-plugins)
   ;; (feature-sfz)
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
   (feature-custom-services
    #:home-services
    (list
     (simple-service
      'change-dwl-guile-configuration
      home-dwl-guile-service-type
      `((setq smart-gaps? #f
              smart-borders? #f
              gaps-oh 30
              gaps-ov 30
              gaps-ih 20
              gaps-iv 10
              border-px 3)))))
   (feature-dwl-guile-monitor-config
    #:rules
    `((set-monitor-rules
       '((name . "DP-2")
         (width . 2560)
         (height . 1440)
         (refresh-rate . 240)
         (adaptive-sync? . #t)))))))
