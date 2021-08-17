(define-module (engstrand features display)
               #:use-module (srfi srfi-1)
               #:use-module (rde features)
               #:use-module (rde features predicates)
               #:use-module (gnu services)
               #:use-module (dwl-guile patches)
               #:use-module (dwl-guile home-service)
               #:use-module (dwl-guile configuration records)
               #:use-module (engstrand utils)
               #:export (feature-dwl-guile-monitor-config))

(define (list-of-monitor-rules? x)
  (every dwl-monitor-rule? x))

(define* (feature-dwl-guile-monitor-config
           #:key
           (monitors '()))
         "Configure monitor settings for dwl-guile."

         (ensure-pred list-of-monitor-rules? monitors)

         (define (get-home-services config)
           "Return a list of home services required for configuring monitors in dwl-guile."
           (when (get-value 'dwl-guile config)
             (list
               (simple-service
                 'add-dwl-guile-monitor-config-patch
                 home-dwl-guile-service-type
                 (modify-dwl-guile
                   (config =>
                           (home-dwl-guile-configuration
                             (inherit config)
                             (patches
                               (append
                                 (home-dwl-guile-configuration-patches config)
                                 (list %patch-monitor-config)))))))
               (simple-service
                 'add-dwl-guile-monitor-rules
                 home-dwl-guile-service-type
                 (modify-dwl-guile-config
                   (config =>
                           (dwl-config
                             (inherit config)
                             (monitor-rules monitors))))))))

         (feature
           (name 'dwl-guile-monitor-config)
           (home-services-getter get-home-services)))
