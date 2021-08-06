(define-module (engstrand reconfigure)
               #:use-module (ice-9 match)
               #:use-module (ice-9 exceptions)
               #:use-module (gnu system)
               #:use-module (rde features)
               #:use-module (engstrand systems)
               #:use-module (engstrand configs)
               #:use-module (engstrand systems p400s)
               #:use-module (engstrand systems tuxedo)
               #:use-module (engstrand systems pavilion)
               #:use-module (engstrand configs johan)
               #:use-module (engstrand configs fredrik))

(define %current-user (getlogin))
(define %current-system (gethostname))

(when (not %current-user)
  (raise-exception
    (make-exception-with-message "reconfigure: could not get user")))

(when (not %current-system)
  (raise-exception
    (make-exception-with-message "reconfigure: could not get hostname")))

(define* (load-feature-list
           #:key
           (prefix "")
           (target "")
           (throw? #t)
           (error-msg ""))
         (let ((var-name (string->symbol (string-append prefix target))))
           (if (defined? var-name)
               (primitive-eval var-name)
               (when throw?
                 (raise-exception
                   (make-exception-with-message error-msg))))))

(define %user-features
  (load-feature-list
    #:prefix "%user-features-"
    #:target %current-user
    #:error-msg
    (string-append "reconfigure: no such user '" %current-user "'")))

(define %system-features
  (load-feature-list
    #:prefix "%system-features-"
    #:target %current-system
    #:error-msg
    (string-append "reconfigure: no such system '" %current-system "'")))

(define %system-swap
  (load-feature-list
    #:prefix "%system-swap-"
    #:target %current-system
    #:throw? #f))

; Check if a swap device has been set in the system configuration.
; If this is the case, we must extend the initial os to make sure
; that it is included in the system configuration.
(define %initial-os
  (if (null? %system-swap)
      %engstrand-initial-os
      (operating-system
        (inherit %engstrand-initial-os)
        (swap-devices %system-swap))))

; All is good, create the configuration
(define generated-config
               (rde-config
                 (initial-os %initial-os)
                 (features
                   (append
                     %user-features
                     %engstrand-base-features
                     %engstrand-system-base-features
                     %system-features))))

(define engstrand-system
               (rde-config-operating-system generated-config))

(define engstrand-he
               (rde-config-home-environment generated-config))

(define (dispatcher)
  (let ((rde-target (getenv "RDE_TARGET")))
    (match rde-target
           ("home" engstrand-he)
           ("system" engstrand-system)
           (_ engstrand-he))))

(dispatcher)
