(define-module (engstrand features radio)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (rde system services accounts)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu home services)
  #:use-module (gnu packages radio)
  #:use-module (engstrand utils)
  #:export (feature-radio))

(define* (feature-radio
          #:key
          (rtl-sdr? #t))

  (define (system-sdr-services _)
    (list
     (simple-service
      'sdr-add-user-groups
      rde-account-service-type
      (list "dialout"))
     (if rtl-sdr?
         (udev-rules-service
          'rtl-sdr-add-udev-rules
          rtl-sdr))))

  (feature
   (name 'radio)
   (values
    `((kernel-arguments-radio
       . ,(list
           (if rtl-sdr? "modprobe.blacklist=dvb_usb_rtl28xxu")))))
   (system-services-getter system-sdr-services)))
