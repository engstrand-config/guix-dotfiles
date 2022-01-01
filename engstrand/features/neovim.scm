(define-module (engstrand features neovim)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (engstrand utils)
  #:use-module (gnu packages vim)
  #:export (feature-neovim))

(define* (feature-neovim)
  "Install and setup Neovim."

  (define (get-home-services config)
    "Return a list of home services required for Neovim"
    (list
     (simple-service
      'add-neovim-home-packages-to-profile
      home-profile-service-type
      (list neovim python-pynvim))))

  (feature
   (name 'neovim)
   (home-services-getter get-home-services)))
