(define-module (engstrand features virtualization)
               #:use-module (rde features)
               #:use-module (rde features predicates)
               #:use-module (gnu services)
               #:use-module (gnu services virtualization)
               #:export (feature-virtualization))

(define* (feature-virtualization
           #:key
           (unix-sock-group "libvirt"))
         "Setup system virtualization."

         (ensure-pred string? unix-sock-group)

         (define (get-system-services config)
           "Return a list of system services required for virtualization."
           (list
            (service virtlog-service-type)
            (service libvirt-service-type
                     (libvirt-configuration
                       (unix-sock-group unix-sock-group)))))

         (feature
           (name 'virtualization)
           (values `((virtualization-groups . ,(list "libvirt" "kvm"))))
           (system-services-getter get-system-services)))
