(define-module (systems system-p400s)
               #:use-module (systems system-base)
               #:use-module (users user-johan)
               #:use-module (gnu))

(operating-system
  (inherit (base-operating-system
           #:user johan
           #:laptop? #f
           #:host-name "p400s"
           #:nix? #t))

  (swap-devices
    (list (uuid "2e1cafae-8f43-4938-be21-fc525250f915")))
  (file-systems
    (cons* (file-system
             (mount-point "/boot/efi")
             (device (uuid "8396-F2E9" 'fat32))
             (type "vfat"))
           (file-system
             (mount-point "/")
             (device
               (uuid "3d72e2f8-6474-4b99-8087-48094ed37f2b"
                     'ext4))
             (type "ext4"))
           %base-file-systems)))
