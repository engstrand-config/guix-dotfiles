(define-module (engstrand features video)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home-services video) ;; rde home-service
  #:use-module (engstrand utils)
  #:export (
            feature-obs
            ))

(define* (feature-obs
          #:key
          (wayland? #t))
  "Setup OBS, the Open Broadcaster Software."

  (define (get-home-services config)
    "Return a list of home services required by OBS."
    (list
     (simple-service
      'add-obs-home-packages-to-profile
      home-profile-service-type
      (let ((base-packages '("obs")))
        (pkgs (if wayland?
                  (cons "obs-wlrobs" base-packages)
                  base-packages))))))

  (feature
   (name 'obs)
   (home-services-getter get-home-services)))
