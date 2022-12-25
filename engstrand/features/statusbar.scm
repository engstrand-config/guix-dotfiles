(define-module (engstrand features statusbar)
  #:use-module (srfi srfi-1)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (engstrand utils)
  #:use-module (farg colorscheme)
  #:use-module (dtao-guile home-service)
  #:export (
            feature-statusbar-dtao-guile

            engstrand-dtao-guile-left-blocks
            engstrand-dtao-guile-center-blocks
            engstrand-dtao-guile-right-blocks

            %engstrand-dtao-guile-config))

(define (engstrand-dtao-guile-left-blocks palette)
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
                    ,(format #f "^bg(~a)^fg(~a)~a^fg()^bg()"
                             (palette 'primary)
                             (make-readable (palette 'primary) (palette 'primary))
                             str))
                   ((dtao:urgent-tag? ,index)
                    ,(format #f "^bg(~a)^fg(~a)~a^fg()^bg()"
                             (palette 'red)
                             (palette 'background) ;; TODO: Light/dark based on theme mode
                             str))
                   ((dtao:active-tag? ,index)
                    ,(format #f "^bg(~a)^fg(~a)~a^fg()^bg()"
                             (offset (palette 'background) 10)
                             (palette 'text)
                             str))
                   (else ,str))))))
    (iota 9 1))
   (list
    (dtao-block
     (events? #t)
     (click `(dtao:next-layout))
     (render `(string-append "^p(4)" (dtao:get-layout)))))))

(define (engstrand-dtao-guile-center-blocks palette)
  (list
   (dtao-block
    (events? #t)
    (render `(dtao:title)))))

(define (engstrand-dtao-guile-right-blocks palette)
  (list
   (dtao-block
    (interval 1)
    (render `(strftime "%A, %d %b (w.%V) %T" (localtime (current-time)))))))

;; TODO: Add options for setting blocks, etc.
(define* (feature-statusbar-dtao-guile)
  "Install and configure dtao-guile."

  ;; Statusbar height
  (define height 25)

  (lambda (fconfig palette)
    (define (get-home-services config)
      "Return a list of home services required by dtao-guile."
      (require-value 'font-monospace config)
      (list
       (service home-dtao-guile-service-type
                (home-dtao-guile-configuration
                 (config
                  (dtao-config
                   (font (font->string 'fcft 'font-monospace config
                                       #:bold? #t))
                   (block-spacing 0)
                   (background-color (palette 'background))
                   (foreground-color (palette 'text))
                   (border-color (palette 'background))
                   (modules '((ice-9 match)
                              (ice-9 popen)
                              (ice-9 rdelim)
                              (srfi srfi-1)))
                   (padding-left 0)
                   (padding-top 0)
                   (padding-bottom 0)
                   (height height)
                   (left-blocks (engstrand-dtao-guile-left-blocks palette))
                   (center-blocks (engstrand-dtao-guile-center-blocks palette))
                   (right-blocks (engstrand-dtao-guile-right-blocks palette))))))))

      (feature
       (name 'statusbar-dtao-guile)
       (values `((statusbar? . #t)
                 (statusbar-height . ,height)
                 (dtao-guile . #t)))
       (home-services-getter get-home-services))))
