(define-module (systems system-tuxedo)
               #:use-module (gnu)
               #:use-module (engstrand packages tuxedo-keyboard-module)
               #:use-module (systems system-base)
               #:use-module (users user-fredrik))

(define %xorg-amdgpu-config
  "Section \"Device\"
  Identifier  \"AMD\"
  Driver      \"amdgpu\"
  Option      \"TearFree\" \"true\"
  Option      \"Backlight\" \"amdgpu_bl0\"
  EndSection")

(operating-system
  (inherit
    (base-operating-system
           #:laptop? #t
           #:user fredrik
           #:host-name "tuxedo"
           #:kernel-modules (list tuxedo-keyboard-module)
           #:xorg-extra (list %xorg-amdgpu-config)
           #:virt? #t
           #:nix? #f))
  (swap-devices
    (list (uuid "40c98866-74b1-4e99-9c32-24d584fe0617")))
  (file-systems
    (cons*
      (file-system
        (mount-point "/boot/efi")
        (device (uuid "7E51-6BDB" 'fat32))
        (type "vfat"))
      (file-system
        (mount-point "/")
        (device
          (uuid "4484aa6c-d5ff-4964-b62d-c2572c701e66"
                'ext4))
        (type "ext4"))
      %base-file-systems)))
