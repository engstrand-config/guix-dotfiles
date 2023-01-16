(define-module (engstrand features networking)
  #:use-module (rde features)
  #:use-module (gnu services)
  #:use-module (gnu services networking)
  #:export (feature-networking))

(define* (feature-networking)
  "Setup networking."

  (define (get-system-services config)
    (list
     (service network-manager-service-type)
     (service wpa-supplicant-service-type)))

  (feature
   (name 'engstrand-networking)
   (system-services-getter get-system-services))))
