(define-module (engstrand systems fractal)
               #:use-module (engstrand utils)
               #:use-module (engstrand systems)
               #:use-module (rde features system)
               #:use-module (gnu system file-systems)
               #:use-module (gnu system mapped-devices))

(define-public %system-swap
	       (list (uuid "876933c5-e607-45cb-b39f-d009b803f41d")))

(define-public %system-features
               (list
                 (feature-host-info
                   #:host-name "fractal"
                   #:timezone %engstrand-timezone
                   #:locale %engstrand-locale)
                 (feature-file-systems
		   #:file-systems
		   (list
		     (file-system
		       (mount-point "/boot/efi")
		       (device (uuid "1A1B-7B25" 'fat32))
		       (type "vfat"))
		     (file-system
		       (mount-point "/")
		       (device
			 (uuid "22d2ae9d-818a-411f-9577-9e6834b02a1d"
			       'ext4))
		       (type "ext4"))))))

