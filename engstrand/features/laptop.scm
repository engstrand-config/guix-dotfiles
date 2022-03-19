(define-module (engstrand features laptop)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (gnu services)
  #:use-module (gnu services pm)
  #:use-module (gnu packages linux)
  #:use-module (dwl-guile home-service)
  #:use-module (engstrand utils)
  #:export (
	    feature-tlp
	    feature-laptop))

(define* (feature-laptop
	  #:key
	  (natural-scrolling? #t))
  "Setup general laptop functionality and configuration."

  (ensure-pred boolean? natural-scrolling?)

  (define (get-home-services config)
    "Return a list of home services required for laptops."
    (make-service-list
     (when (get-value 'dwl-guile config)
       (simple-service
	'set-natural-scrolling-in-dwl-guile
	home-dwl-guile-service-type
	(modify-dwl-guile-config
	 (config =>
		 (dwl-config
		  (inherit config)
		  (natural-scrolling? natural-scrolling?))))))))

  (feature
   (name 'laptop)
   (values `((laptop . #t)))
   (home-services-getter get-home-services)))

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
