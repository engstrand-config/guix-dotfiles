(define-module (engstrand features display)
  #:use-module (ice-9 format)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages linux)
  #:use-module (dwl-guile utils)
  #:use-module (dwl-guile patches)
  #:use-module (dwl-guile home-service)
  #:use-module (dwl-guile configuration)
  #:use-module (dwl-guile configuration records)
  #:use-module (engstrand utils)
  #:export (
            feature-dwl-guile-monitor-config
            feature-kanshi-autorandr))

;;  TODO: requires package ddcutil from (gnu packages hardware)
;;  please check if package i2c-tools from (gnu packages linux) is required
;;  TODO: add kernel module i2c-dev
;;  TODO: add user to group i2c-dev
;;  TODO: guile bindings for ddcutil?
;;  example commands that we have used before:
;;  ddcutil setvcp 10 $(echo -e "0\n50\n100" | bemenu -i -p "Set monitor brightness level:")
;;  or (increases brightness by 5 %):
;;  ddcutil setvcp 10 + 5
;;  )

(define (list-of-monitor-rules? x)
  (every dwl-monitor-rule? x))

(define* (feature-dwl-guile-monitor-config
          #:key
          (monitors '())
          (focus-left-key "s-<left>")
          (focus-right-key "s-<right>")
          (move-left-key "S-s-<left>")
          (move-right-key "S-s-<right>")
          (add-keybindings? #t))
  "Configure monitor settings for dwl-guile."

  (ensure-pred list-of-monitor-rules? monitors)
  (ensure-pred string? focus-left-key)
  (ensure-pred string? focus-right-key)
  (ensure-pred string? move-left-key)
  (ensure-pred string? move-right-key)

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
                    (list %patch-monitor-config
                          %patch-focusmonpointer)))))))
       (simple-service
        'add-dwl-guile-monitor-rules
        home-dwl-guile-service-type
        (modify-dwl-guile-config
         (config =>
                 (dwl-config
                  (inherit config)
                  (monitor-rules monitors)))))
       (when add-keybindings?
         (simple-service
          'add-dwl-guile-monitor-rules
          home-dwl-guile-service-type
          (modify-dwl-guile-config
           (config =>
                   (dwl-config
                    (inherit config)
                    (monitor-rules monitors)
                    (keys
                     (append
                      (list
                       (dwl-key
                        (key focus-left-key)
                        (action `(dwl:focus-monitor DIRECTION-LEFT)))
                       (dwl-key
                        (key focus-right-key)
                        (action `(dwl:focus-monitor DIRECTION-RIGHT)))
                       (dwl-key
                        (key move-left-key)
                        (action `(dwl:tag-monitor DIRECTION-LEFT)))
                       (dwl-key
                        (key move-right-key)
                        (action `(dwl:tag-monitor DIRECTION-RIGHT))))
                      (dwl-config-keys config)))))))))))

  (feature
   (name 'dwl-guile-monitor-config)
   (home-services-getter get-home-services)))

(define* (feature-kanshi-autorandr
          #:key
          (auto-start? #t)
          (profiles '()))
  "Configure kanshi for automatically enable and disabled outputs on hotplug."

  (ensure-pred boolean? auto-start?)
  (ensure-pred list-of-list? profiles)

  (define (serialize-output-fields fields)
    (string-join
     (map
      (lambda (field)
        (let ((value (cdr field)))
          (format #f "~a ~a"
                  (car field)
                  (cond
                   ((number? field) (number->string value))
                   (else value)))))
      fields)))

  (define (serialize-output output acc)
    (string-append acc (format #f "\toutput ~s ~a\n"
                               (car output)
                               (serialize-output-fields (cdr output)))))

  (define (serialize-profile profile acc)
    (string-append acc "profile {\n" (fold serialize-output "" profile) "}\n\n"))

  (define (serialize-profiles profiles)
    (fold serialize-profile "" profiles))

  (define (get-home-services config)
    "Return a list of home services required by kanshi."
    (let ((has-dwl-guile? (get-value 'dwl-guile config)))
      (list
       (simple-service
        'add-kanshi-home-packages-to-profile
        home-profile-service-type
        (list kanshi))
       (simple-service
        'create-kanshi-config
        home-files-service-type
        `((".config/kanshi/config"
           ,(plain-file "kanshi-config" (serialize-profiles profiles)))))
       (simple-service
        'add-kanshi-shepherd-service
        home-shepherd-service-type
        (list
         (shepherd-service
          (documentation "Run kanshi autorandr.")
          (provision '(kanshi))
          (requirement (if has-dwl-guile? '(dwl-guile) '()))
          (auto-start? auto-start?)
          (respawn? #t)
          (start
           #~(make-forkexec-constructor
              (list
               #$(file-append kanshi "/bin/kanshi"))
              #:log-file #$(make-log-file "kanshi")))
          (stop #~(make-kill-destructor))))))))

  (feature
   (name 'kanshi)
   (home-services-getter get-home-services)))
