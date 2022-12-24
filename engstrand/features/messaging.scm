(define-module (engstrand features messaging)
  #:use-module (rde features)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (nongnu packages messaging)
  #:export (feature-signal))

(define* (feature-signal)
  "Install the Signal desktop client."

  (define (get-home-services config)
    (list
     (simple-service
      'add-signal-home-packages-to-profile
      home-profile-service-type
      (list signal-desktop))))

  (feature
   (name 'signal)
   (home-services-getter get-home-services)))
