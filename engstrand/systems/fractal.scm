(define-module (engstrand systems fractal)
  #:use-module (engstrand utils)
  #:use-module (engstrand systems)
  #:use-module (engstrand features display)
  #:use-module (engstrand features dwl-guile)
  #:use-module (engstrand features bluetooth)
  #:use-module (rde features system)
  #:use-module (rde features bluetooth)
  #:use-module (guix gexp)
  #:use-module (dwl-guile home-service)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system mapped-devices))

(define %mapped-devices
  (list
   (mapped-device
    (source
     (uuid "367c5fe8-0388-49ad-9c88-04bcfe62c7b9"))
    (target "cryptroot")
    (type luks-device-mapping))))

(define-public %system-features
  (list
   (feature-host-info
    #:host-name "fractal"
    #:timezone %engstrand-timezone
    #:locale %engstrand-locale)
   (feature-bootloader
    #:bootloader-configuration
    (bootloader-configuration
     (bootloader grub-efi-bootloader)
     (targets '("/boot/efi"))
     (keyboard-layout %engstrand-keyboard-layout)))
   (feature-file-systems
    #:mapped-devices %mapped-devices
    #:file-systems
    (list
     (file-system
      (mount-point "/boot/efi")
      (device (uuid "1A1B-7B25" 'fat32))
      (type "vfat"))
     (file-system
      (mount-point "/")
      (device "/dev/mapper/cryptroot")
      (type "ext4")
      (dependencies %mapped-devices))))
   (feature-bluetooth)
   (feature-bluetooth-quick-connect)
   (feature-display-control
    #:decrease-brightness-key "s-<F1>"
    #:increase-brightness-key "s-<F2>")
   (feature-dwl-guile-custom-config
    #:config
    `((setq accel-speed -0.7)
      (set-monitor-rules
       '((name "DP-3")
         (width . 1920)
         (height . 1080)
         (refresh-rate . 144)
         ;; Adaptive sync is broken on this display.
         ;; Enabling it will result in a black screen.
         (adaptive-sync? . #f)))))))
