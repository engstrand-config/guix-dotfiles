(define-module (engstrand reconfigure)
               #:use-module (ice-9 match)
               #:use-module (gnu system)
               #:use-module (rde features)
               #:use-module (engstrand systems)
               #:use-module (engstrand configs))

; Extract current config and system based on username and hostname.
(define %current-user (getenv "USER"))
(define %current-system (gethostname))

; Make sure that both variables exist
(when (not %current-user) (throw 'no-user . (display "$USER not set")))
(when (not %current-system) (throw 'no-hostname' . (display "$HOST not set")))

(define (dynamic-load submodule module-exp)
  (primitive-eval `(use-modules (engstrand
                                  ,(string->symbol submodule)
                                  ,(string->symbol module-exp)))))

; Load configuration
; TODO: How do we validate the result of this?
(dynamic-load "configs" %current-user)
(dynamic-load "systems" %current-system)

; Make sure that the imported modules exports the necessary variables
; TODO: Access these in some other way? Perhaps return the feature list
;       directly and receive the features when loading the module?
(when (not %user-features) (throw 'no-config-features . (display "'%user-features' is not defined")))
(when (not %system-features) (throw 'no-system-features . (display "'%system-features' is not defined")))

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
(define-public generated-config
               (rde-config
                 (initial-os %initial-os)
                 (features
                   (append
                     %user-features
                     %config-base-features
                     %system-base-features
                     %system-features))))

(define-public engstrand-system
               (rde-config-operating-system generated-config))

(define-public engstrand-he
               (rde-config-home-environment generated-config))

(define (dispatcher)
  (let ((rde-target (getenv "RDE_TARGET")))
    (match rde-target
           ("home" engstrand-he)
           ("system" engstrand-system)
           (_ engstrand-he))))

(dispatcher)
