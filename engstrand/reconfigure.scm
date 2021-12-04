(define-module (engstrand reconfigure)
               #:use-module (ice-9 match)
               #:use-module (ice-9 exceptions)
               #:use-module (ice-9 pretty-print)
               #:use-module (gnu system)
               #:use-module (gnu system accounts)
               #:use-module (rde features)
               #:use-module (rde features predicates)
               #:use-module (engstrand systems)
               #:export (make-config))

; Allows dynamic loading of configuration modules based on file name.
(define* (dynamic-load sub mod var-name #:key (throw? #t))
         (let ((var (module-variable
                      (resolve-module `(engstrand ,sub ,(string->symbol mod))) var-name)))
           (if (or (not var) (not (variable-bound? var)))
               (when throw?
                 (raise-exception
                   (make-exception-with-message
                     (string-append "reconfigure: could not load module '" mod "'"))))
               (variable-ref var))))

; Create a system or home configuration based on some parameters.
; Generally, you want to call this procedure with no arguments.
(define* (make-config
           #:key
           (user (getenv "RDE_USER"))
           (system (gethostname))
           (target (getenv "RDE_TARGET"))
           (initial-os %engstrand-initial-os))

         (ensure-pred string? user)
         (ensure-pred string? system)
         (ensure-pred operating-system? initial-os)

         (define %user-features (dynamic-load 'configs user '%user-features))
         (define %system-features (dynamic-load 'systems system '%system-features))
         (define %system-swap (dynamic-load 'systems system '%system-swap #:throw? #f))

         ; Check if a swap device has been set in the system configuration.
         ; If this is the case, we must extend the initial os to make sure
         ; that it is included in the system configuration.
         (define %initial-os
           (if (or (unspecified? %system-swap) (null? %system-swap))
               initial-os
               (operating-system
                 (inherit initial-os)
                 (swap-devices %system-swap))))

         ; All is good, create the configuration
         (define %generated-config
           (rde-config
             (initial-os %initial-os)
             (features
               (append
                 %user-features
                 %engstrand-system-base-features
                 %system-features))))

         (define %engstrand-he
           (rde-config-home-environment %generated-config))

         (define %engstrand-system
           (let ((os (rde-config-operating-system %generated-config)))
             (operating-system
               (inherit os)
               (users
                 (map
                   (lambda (user)
                     (user-account
                       (inherit user)
                       (supplementary-groups
                         ; Temporary hack to allow user to run VMs
                         '("wheel" "netdev" "audio" "video" "libvirt" "kvm" "lp"))))
                   (operating-system-users os))))))

         (match target
                ("home" %engstrand-he)
                ("system" %engstrand-system)
                (_ %engstrand-system)))
