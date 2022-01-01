(define-module (engstrand features statusbar)
  #:use-module (srfi srfi-1)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (engstrand utils)
  #:use-module (dtao-guile home-service)
  #:export (
            feature-statusbar-dtao-guile

            %engstrand-dtao-guile-config
            %engstrand-dtao-guile-left-blocks
            %engstrand-dtao-guile-center-blocks
            %engstrand-dtao-guile-right-blocks))

(define %engstrand-dtao-guile-left-blocks
  (list
   (dtao-block
    (events? #t)
    (render `(dtao:title)))))

(define %engstrand-dtao-guile-center-blocks
  (map
   (lambda (tag)
     (dtao-block
      (interval 0)
      (events? #t)
      (render
       `(format #f "~a^p(8)~a^p(8)^fg()^bg()"
                (let ((index (- ,tag 1)))
                  (cond
                   ((dtao:selected-tag? index) "^bg(#ffcc00)^fg(#191919)")
                   ((dtao:urgent-tag? index) "^bg(#ff0000)^fg(#ffffff)")
                   ((dtao:active-tag? index) "^bg(#323232)^fg(#ffffff)")
                   (else "")))
                ,(number->string tag)))))
   (iota 9 1)))

(define %engstrand-dtao-guile-right-blocks
  (list
   (dtao-block
    (interval 1)
    (render `(strftime "%A, %d %b (w.%W) %T" (localtime (current-time)))))))

;; General dtao-guile configuration
(define %engstrand-dtao-guile-config
  (dtao-config
   (block-spacing 0)
   (bottom? #t)
   (use-dwl-guile-colorscheme? #t)
   (left-blocks %engstrand-dtao-guile-left-blocks)
   (center-blocks %engstrand-dtao-guile-center-blocks)
   (right-blocks %engstrand-dtao-guile-right-blocks)))

(define* (feature-statusbar-dtao-guile
          #:key
          (dtao-guile-configuration (home-dtao-guile-configuration)))
  "Install and configure dtao-guile."

  (ensure-pred home-dtao-guile-configuration? dtao-guile-configuration)

  (define (get-home-services config)
    "Return a list of home services required by dtao-guile."
    (list
     (service home-dtao-guile-service-type
              dtao-guile-configuration)))

  (feature
   (name 'statusbar-dtao-guile)
   (values `((statusbar? . #t)
             (dtao-guile . #t)))
   (home-services-getter get-home-services)))
