(define-module (engstrand systems ghost)
	       #:use-module (engstrand utils)
	       #:use-module (engstrand systems)
	       #:use-module (engstrand features display)
               #:use-module (engstrand features bluetooth)
	       #:use-module (rde features system)
	       #:use-module (gnu bootloader)
	       #:use-module (gnu bootloader grub)
	       #:use-module (dwl-guile home-service)
	       #:use-module (gnu system file-systems)
	       #:use-module (gnu system mapped-devices))

(define %mapped-devices
  (list
    (mapped-device
      (source
	(uuid "baeef44b-6ca9-4fbb-ad80-f45f35724d32"))
      (target "cryptroot")
      (type luks-device-mapping))))

(define-public %system-features
	       (list
		 (feature-host-info
		   #:host-name "ghost"
		   #:timezone %engstrand-timezone
		   #:locale %engstrand-locale)
		 (feature-bootloader
		   #:bootloader-configuration
		   (bootloader-configuration
		     (bootloader grub-bootloader)
		     (targets '("/dev/nvme0n1"))
                     (keyboard-layout %engstrand-keyboard-layout)))
                 (feature-bluetooth)
                 (feature-file-systems
                   #:mapped-devices %mapped-devices
                   #:file-systems
                   (list
                     (file-system
                       (mount-point "/boot/efi")
                       (device (uuid "F6B6-B394" 'fat32))
                       (type "vfat"))
                     (file-system
                       (mount-point "/")
                       (device "/dev/mapper/cryptroot")
                       (type "ext4")
                       (dependencies %mapped-devices))))
                 (feature-dwl-guile-monitor-config
                   #:monitors
                   (list
                     (dwl-monitor-rule
                       (name "DP-2")
                       (width 2560)
                       (height 1440)
                       (refresh-rate 240)
                       (adaptive-sync? #t))))))
