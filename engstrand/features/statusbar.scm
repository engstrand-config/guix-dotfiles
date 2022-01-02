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
  (append
   (map
    (lambda (tag)
      (let ((str (string-append "^p(8)" (number->string tag) "^p(8)"))
            (index (- tag 1)))
      (dtao-block
       (interval 0)
       (events? #t)
       (click `(match button
                 (0 (dtao:view ,index))))
       (render `(cond
                 ((dtao:selected-tag? ,index)
                  ,(string-append "^bg(#ffcc00)^fg(#191919)" str "^fg()^bg()"))
                 ((dtao:urgent-tag? ,index)
                  ,(string-append "^bg(#ff0000)^fg(#ffffff)" str "^fg()^bg()"))
                 ((dtao:active-tag? ,index)
                  ,(string-append "^bg(#323232)^fg(#ffffff)" str "^fg()^bg()"))
                 (else ,str))))))
    (iota 9 1))
   (list
    (dtao-block
     (events? #t)
     (click `(dtao:next-layout))
     (render `(string-append "^p(4)" (dtao:get-layout)))))))

(define %engstrand-dtao-guile-center-blocks
  (list
   (dtao-block
    (events? #t)
    (render `(dtao:title)))))

(define %engstrand-dtao-guile-right-blocks
  (list
   (dtao-block
    (interval 1)
    (render `(strftime "%A, %d %b (w.%V) %T" (localtime (current-time)))))))

;; General dtao-guile configuration
(define %engstrand-dtao-guile-config
  (dtao-config
   (block-spacing 0)
   (use-dwl-guile-colorscheme? #t)
   (modules '((ice-9 match)))
   (padding-left 0)
   (padding-top 0)
   (padding-bottom 0)
   (height 25)
   (left-blocks %engstrand-dtao-guile-left-blocks)
   (center-blocks %engstrand-dtao-guile-center-blocks)
   (right-blocks %engstrand-dtao-guile-right-blocks)))

(define* (feature-statusbar-dtao-guile
          #:key
          (dtao-guile-configuration (home-dtao-guile-configuration)))
  "Install and configure dtao-guile."

  (ensure-pred home-dtao-guile-configuration? dtao-guile-configuration)

  ;; dtao-config
  (define dtao
    (home-dtao-guile-configuration-config dtao-guile-configuration))

  (define (get-home-services config)
    "Return a list of home services required by dtao-guile."
    (list
     (service home-dtao-guile-service-type
              dtao-guile-configuration)))

  (feature
   (name 'statusbar-dtao-guile)
   (values `((statusbar? . #t)
             (statusbar-height . ,(dtao-config-height dtao))
             (dtao-guile . #t)))
   (home-services-getter get-home-services)))
