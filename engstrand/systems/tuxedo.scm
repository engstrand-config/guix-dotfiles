(define-module (engstrand systems tuxedo)
  #:use-module (engstrand utils)
  #:use-module (engstrand systems)
  #:use-module (engstrand packages linux)
  #:use-module (engstrand features laptop)
  #:use-module (engstrand features display)
  #:use-module (engstrand features bluetooth)
  #:use-module (guix gexp)
  #:use-module (dwl-guile home-service)
  #:use-module (rde features bluetooth)
  #:use-module (rde features system)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (gnu packages linux)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system mapped-devices))

(define-public %system-features
  (append
   (list
    ;; TODO: Changing a single value in this feature requires
    ;;       you to define the entire feature again. Perhaps add a helper for this?
    (feature-kernel
     #:kernel linux
     #:firmware (list linux-firmware)
     #:kernel-arguments %engstrand-kernel-arguments
     #:kernel-loadable-modules
     (append (list v4l2loopback-linux-module)
             (kernel-modules->list (list tuxedo-keyboard-module)
                                   linux)))
    (feature-host-info
     #:host-name "tuxedo"
     #:timezone %engstrand-timezone
     #:locale %engstrand-locale)
    (feature-bootloader)
    (feature-file-systems
     #:file-systems
     (list
      (file-system
       (mount-point "/boot/efi")
       (device (uuid "7E51-6BDB" 'fat32))
       (type "vfat"))
      (file-system
       (mount-point "/")
       (device
        (uuid "4484aa6c-d5ff-4964-b62d-c2572c701e66" 'ext4))
       (type "ext4")))
     #:swap-devices
     (list
      (swap-space
       (target (uuid "40c98866-74b1-4e99-9c32-24d584fe0617")))))
    (feature-bluetooth)
    (feature-bluetooth-quick-connect)
    (feature-dwl-guile-monitor-config
     #:rules
     `((set-monitor-rules
        '((name . "eDP-1")
          (width . 1920)
          (height . 1080)
          (refresh-rate . 60)
          (adaptive-sync? . #f)))))
    (feature-kanshi-autorandr
     #:profiles
     '((("Ancor Communications Inc MG248 G6LMQS123017" .
         (("mode" . "1920x1080")
          ("position" . "0,0")))
        (eDP-1 . (("position" . "0,1080")))))))
   %engstrand-laptop-base-features))
