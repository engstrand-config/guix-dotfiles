(define-module (engstrand systems p400s)
  #:use-module (engstrand utils)
  #:use-module (engstrand systems)
  #:use-module (engstrand features display)
  #:use-module (guix gexp)
  #:use-module (rde features system)
  #:use-module (dwl-guile home-service)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system mapped-devices))

(define-public %system-features
  (list
   (feature-host-info
    #:host-name "p400s"
    #:timezone %engstrand-timezone
    #:locale %engstrand-locale)
   (feature-bootloader)
   (feature-file-systems
    #:file-systems
    (list (file-system
           (mount-point "/boot/efi")
           (device (uuid "8396-F2E9" 'fat32))
           (type "vfat"))
          (file-system
           (mount-point "/")
           (device
            (uuid "104e5086-1795-4a28-b3eb-f563ef06fc52"
                  'ext4))
           (type "ext4")))
    #:swap-devices
    (list (swap-space
           (target (uuid "5fdc125b-0a5f-4706-8b8f-82b783979d03")))))
   (feature-dwl-guile-monitor-config
    #:rules
    `((set-monitor-rules
       '((name "DP-1")
         (width 2560)
         (height 1440)
         (refresh-rate 60)
         (adaptive-sync? #f)))))))
