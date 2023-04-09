(define-module (engstrand features theming)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (rde features)
  #:use-module (farg home-service)
  #:export (feature-farg))

(define* (feature-farg)
  "Installs and configures farg, a system colorscheme manager for Guix."

  (lambda (fsource _)
    (define (get-home-services config)
      "Return a list of home services required by farg"
      (list
       (service home-farg-service-type
                (home-farg-configuration
                 (source fsource)))))

    (feature
     (name 'farg)
     (values '(("farg" . #t)))
     (home-services-getter get-home-services))))
