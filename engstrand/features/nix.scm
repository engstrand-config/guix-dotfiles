(define-module (engstrand features nix)
               #:use-module (rde features)
               #:use-module (rde features predicates)
               #:use-module (gnu services)
               #:use-module (gnu services nix)
               #:use-module (engstrand utils)
               #:export (feature-nix))

(define* (feature-nix)
         "Setup the nix package manager."

         (define (get-system-services config)
           "Return a list of system services required by nix."
           (list
             (simple-service
               'add-nix-system-packages-to-profile
               profile-service-type
               (pkgs '("nix")))
             (service nix-service-type)))

         (feature
           (name 'nix)
           (system-services-getter get-system-services)))
