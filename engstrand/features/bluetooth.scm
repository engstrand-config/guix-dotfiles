(define-module (engstrand features bluetooth)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (gnu packages linux)
  #:use-module (gnu services desktop)
  #:use-module (engstrand utils)
  #:use-module (rde system services accounts)
  #:export (feature-bluetooth))

(define* (feature-bluetooth
          #:key
          (auto-enable? #t))
  "Configure and run the bluetoothd daemon."

  (define (get-system-services config)
    (list
     (simple-service
      'bluetooth-add-user-groups
      rde-account-service-type
      (list "lp"))
     (bluetooth-service
      #:bluez bluez
      #:auto-enable? auto-enable?)))

  (feature
   (name 'bluetooth)
   (system-services-getter get-system-services)))
