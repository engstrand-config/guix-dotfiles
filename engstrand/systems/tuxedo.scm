(define-module (engstrand systems tuxedo)
               #:use-module (engstrand utils)
               #:use-module (engstrand systems)
               #:use-module (engstrand packages tuxedo-keyboard-module)
               #:use-module (rde features system)
               #:use-module (nongnu packages linux)
               #:use-module (nongnu system linux-initrd)
               #:use-module (gnu system file-systems)
               #:use-module (gnu system mapped-devices))

; TODO: Add amdgpu driver for xorg as a feature
; (define %xorg-amdgpu-config
;   "Section \"Device\"
;   Identifier  \"AMD\"
;   Driver      \"amdgpu\"
;   Option      \"TearFree\" \"true\"
;   Option      \"Backlight\" \"amdgpu_bl0\"
;   EndSection")

; TODO: Add support for swap-devices as a feature.
; rde does not support this out of the box. Instead, we
; must pass it using the initial-os field of rde-config.
(define-public %system-swap
               (list (uuid "40c98866-74b1-4e99-9c32-24d584fe0617")))

(define-public %system-features
               (list
                 (feature-kernel
                   #:kernel linux
                   #:firmware (list linux-firmware)
                   #:kernel-arguments %engstrand-kernel-arguments
                   #:kernel-loadable-modules (kernel-modules->list (list tuxedo-keyboard-module)
                                                                   linux))
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
                       (type "ext4"))))))
