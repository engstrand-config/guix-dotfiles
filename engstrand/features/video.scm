(define-module (engstrand features video)
               #:use-module (rde features)
               #:use-module (rde features predicates)
               #:use-module (gnu home-services)
               #:use-module (gnu home-services video)
               #:use-module (engstrand utils)
               #:export (
                         feature-mpv
                         %engstrand-mpv-configuration))

; Allows each user to easily extend a shared default configuration
(define %engstrand-mpv-configuration
  (home-mpv-configuration))

(define* (feature-mpv
           #:key
           (mpv-configuration %engstrand-mpv-configuration))
         "Setup the mpv video player."

         (define (get-home-services config)
           "Return a list of home services required by mpv."
           (list
             (service home-mpv-service-type mpv-configuration)))

         (feature
           (name 'mpv)
           (home-services-getter get-home-services)))
