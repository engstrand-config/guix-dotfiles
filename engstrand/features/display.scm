(define-module (engstrand features display)
  #:use-module (ice-9 format)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (rde system services accounts)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages hardware)
  #:use-module (gnu packages linux)
  #:use-module (dwl-guile utils)
  #:use-module (dwl-guile patches)
  #:use-module (dwl-guile home-service)
  #:use-module (engstrand utils)
  #:use-module (engstrand features wayland)
  #:export (feature-display-control
            feature-kanshi-autorandr))

(define* (feature-display-control
          #:key
          (step-brightness 10)
          (increase-brightness-key "<XF86MonBrightnessUp>")
          (decrease-brightness-key "<XF86MonBrightnessDown>")
          (add-keybindings? #t))

  (define (get-system-services _)
    (list
     (simple-service
      'ddcutil-add-i2c-group-to-user
      rde-account-service-type
      (list "i2c"))
     (udev-rules-service
      'ddcutil-add-udev-rules-group
      ddcutil
      #:groups '("i2c"))))

  (define command (file-append ddcutil "/bin/ddcutil"))

  (define (get-home-services config)
    "Return a list of home services required by ddcutil."
    (let ((has-dwl-guile? (get-value 'dwl-guile config)))
      (list
       (simple-service
        'add-ddcutil-home-package-to-profile
        home-profile-service-type
        (list ddcutil))
       (when (and add-keybindings? has-dwl-guile?)
         (simple-service
          'add-ddcutil-dwl-keybindings
          home-dwl-guile-service-type
          `((set-keys ,increase-brightness-key
                      (lambda ()
                        (dwl:shcmd ,command
                                   "setvcp" "10"
                                   "+"
                                   ,(number->string step-brightness)))
                      ,decrease-brightness-key
                      (lambda ()
                        (dwl:shcmd ,command
                                   "setvcp" "10"
                                   "-"
                                   ,(number->string step-brightness))))))))))

  (feature
   (name 'display-control)
   (home-services-getter get-home-services)
   (system-services-getter get-system-services)))

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
