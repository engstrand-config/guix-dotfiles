(define-module (engstrand systems p400s)
               #:use-module (engstrand utils)
               #:use-module (engstrand systems)
               #:use-module (engstrand features display)
               #:use-module (rde features system)
	       #:use-module (dwl-guile home-service)
               #:use-module (gnu system file-systems)
               #:use-module (gnu system mapped-devices))

(define-public %system-swap
               (list (uuid "2e1cafae-8f43-4938-be21-fc525250f915")))

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
                             (uuid "3d72e2f8-6474-4b99-8087-48094ed37f2b"
                                   'ext4))
                           (type "ext4"))))
                 (feature-dwl-guile-monitor-config
                   #:monitors
                   (list
                     (dwl-monitor-rule
                       (name "DP-1")
                       (width 2560)
                       (height 1440)
                       (refresh-rate 120)
                       (adaptive-sync? #t))))))
