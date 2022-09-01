(define-module (engstrand features base)
  #:use-module (farg config)
  #:use-module (farg home-service)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu home services)
  #:use-module (gnu packages linux)
  #:use-module (engstrand utils)
  #:export (
            feature-switch-to-tty-on-boot
            feature-farg))

(define* (feature-switch-to-tty-on-boot
          #:key
          (tty 2))
  "Switch to TTY on boot, after base services has been started."

  (ensure-pred number? tty)

  (define (get-system-services config)
    "Return a list of system services required for switching tty."
    (list
     (simple-service
      'switch-tty-after-boot
      shepherd-root-service-type
      (list (shepherd-service
             (provision '(startup-tty))
             (requirement '(user-processes user-file-systems host-name virtual-terminal udev))
             (start #~(lambda ()
                        (invoke #$(file-append kbd "/bin/chvt")
                                #$(format #f "~a" tty))))
             (one-shot? #t))))))

  (feature
   (name 'switch-tty-after-boot)
   (system-services-getter get-system-services)))

(define* (feature-farg)
  "Installs and configures farg, a system colorscheme manager for Guix."

  (lambda (config palette)
    (define (get-home-services config)
      "Return a list of home services required by farg"
      (list
       (service home-farg-service-type config)))

    (feature
     (name 'farg)
     (home-services-getter get-home-services))))
