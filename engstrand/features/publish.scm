(define-module (engstrand features publish)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (gnu packages package-management)
  #:use-module (gnu services base)
  #:use-module (gnu services)
  ;; #:use-module (gnu services shepherd)
  ;; #:use-module (gnu home services)
  ;; #:use-module (engstrand utils)
  #:export (feature-publish))

(define* (feature-publish
          #:key
          (guix guix)
          (port 80)
          (host "localhost")
          (advertise? #f)
          (compress? #t)
          (compression-methods '(("gzip" 3) ("zstd" 3)))
          (nar-path "nar")
          (cache #f)
          (workers #f)
          (cache-bypass-threshold 1000000)
          (ttl #f))
  "Service to publish substitutes of Guix packages on the network."

  (ensure-pred number? port)
  (ensure-pred string? host)
  (ensure-pred boolean? advertise?)
  (ensure-pred boolean? compress?)
  (ensure-pred string? nar-path)
  ; (ensure-pred number? cache)
  ; (ensure-pred number? workers)
  (ensure-pred number? cache-bypass-threshold)

  (define (get-system-services config)
    (list
     (service guix-publish-service-type
              (guix-publish-configuration
               (guix guix)
               (port port)
               (host host)
               (advertise? advertise?)
               (compression (if compress?
                                compression-methods
                                '()))
               (nar-path nar-path)
               (cache cache)
               (workers workers)
               (cache-bypass-threshold cache-bypass-threshold)))))

  (feature
   (name 'publish)
   (system-services-getter get-system-services)))
