(define-module (engstrand reconfigure)
               #:use-module (ice-9 match)
               #:use-module (ice-9 exceptions)
               #:use-module (ice-9 pretty-print)
               #:use-module (gnu system)
               #:use-module (rde features)
               #:use-module (engstrand systems)
               #:use-module (engstrand configs)
               #:use-module (engstrand systems p400s)
               #:use-module (engstrand systems tuxedo)
               #:use-module (engstrand systems pavilion)
               #:use-module (engstrand configs johan)
               #:use-module (engstrand configs fredrik))

; Primarily use "RDE_USER" environment variable,
; but fallback to the currently logged in user.
; Note that using sudo should not affect the result
; of (getlogin). It might still fail to find the user though.
(define %current-user
  (let ((rde-user (getenv "RDE_USER")))
    (if (not rde-user) (getlogin) rde-user)))

(define %current-system (gethostname))

(when (not %current-user)
  (raise-exception
    (make-exception-with-message "reconfigure: could not get user")))

(when (not %current-system)
  (raise-exception
    (make-exception-with-message "reconfigure: could not get hostname")))

(define* (dynamic-load sub mod var-name #:key (throw? #t))
  (let ((var (module-variable (resolve-module `(engstrand ,sub ,(string->symbol mod))) var-name)))
    (if (or (not var) (not (variable-bound? var)))
        (when throw?
          (raise-exception
            (make-exception-with-message
              (string-append "reconfigure: could not load module '" mod "'"))))
        (variable-ref var))))

(define %user-features (dynamic-load 'configs %current-user '%user-features))
(define %system-features (dynamic-load 'systems %current-system '%system-features))
(define %system-swap (dynamic-load 'systems %current-system '%system-swap #:throw? #f))

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
