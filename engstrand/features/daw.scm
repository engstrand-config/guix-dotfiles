(define-module (engstrand features daw)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages music)
  #:use-module (engstrand utils)
  #:export (feature-ardour
            feature-zrythm
            feature-calf-plugins
            feature-sfz))

;; TODO: Set up JACK? Separate feature?

(define* (feature-ardour)
  "Install and configure the Ardour DAW."

  (define (get-home-services config)
    (make-service-list
     (simple-service
      'add-ardour-package-to-profile
      home-profile-service-type
      (list ardour))))

  (feature
   (name 'ardour)
   (home-services-getter get-home-services)))

(define* (feature-zrythm)
  "Install and configure the Zrythm DAW."

  (define (get-home-services config)
    (make-service-list
     (simple-service
      'add-zrythm-package-to-profile
      home-profile-service-type
      (list zrythm))))

  (feature
   (name 'zrythm)
   (home-services-getter get-home-services)))

(define* (feature-calf-plugins)
  "Install the Calf Studio Gear LV2 plugins."

  (define (get-home-services config)
    (make-service-list
     (simple-service
      'add-calf-package-to-profile
      home-profile-service-type
      (list calf))))

  (feature
   (name 'calf-plugins)
   (home-services-getter get-home-services)))

(define* (feature-sfz)
  "Install SFZ sampler/parser plugins."

  (define (get-home-services config)
    (make-service-list
     (simple-service
      'add-sfz-package-to-profile
      home-profile-service-type
      (list sfizz liquidsfz))))

  (feature
   (name 'sfz)
   (home-services-getter get-home-services)))
