(define-module (engstrand features shells)
  #:use-module (rde gexp)
  #:use-module (rde features)
  #:use-module (rde packages)
  #:use-module (rde features predicates)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages shellutils)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu home-services shells)
  #:use-module (gnu home-services shellutils)
  #:use-module (engstrand utils)
  #:export (feature-zsh))

(define* (feature-zsh)
  "Setup zsh."

  (define (get-home-services config)
    "Return a list of home services required by zsh"
    (list
     (service home-zsh-autosuggestions-service-type
              zsh-autosuggestions)
     (service home-zsh-service-type
              (home-zsh-configuration
               ;; TODO: Use absolute paths
               (zshrc `(,(slurp-file-like (local-file "../files/zshrc"))))
               (zprofile `(,(slurp-file-like (local-file "../files/shell-profile"))))))))
  (feature
   (name 'zsh)
   (values `((login-shell . ,(file-append zsh "/bin/zsh"))))
   (home-services-getter get-home-services)))
