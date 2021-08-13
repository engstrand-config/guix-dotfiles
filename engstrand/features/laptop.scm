(define-module (engstrand features laptop)
               #:use-module (rde features)
               #:use-module (rde features predicates)
               #:use-module (gnu services)
               #:use-module (gnu services pm)
               #:use-module (gnu packages linux)
               #:use-module (engstrand utils)
               #:export (feature-tlp))

(define* (feature-tlp
           #:key
           (tlp-config (tlp-configuration
                         (cpu-scaling-governor-on-ac (list "performance"))
                         (sched-powersave-on-bat? #t))))
         "Setup TLP for power management on laptops."

         (define (get-system-services config)
           "Return a list of system services required by TLP"
           (list
             (simple-service
               'add-tlp-system-packages-to-profile
               profile-service-type
               (pkgs '("tlp")))
             (service tlp-service-type
                      tlp-config)))

         (feature
           (name 'tlp)
           (system-services-getter get-system-services)))
