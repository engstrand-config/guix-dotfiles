(define-module (engstrand features laptop)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu services pm)
  #:use-module (gnu packages linux)
  #:use-module (dwl-guile utils)
  #:use-module (dwl-guile patches)
  #:use-module (dwl-guile home-service)
  #:use-module (dwl-guile configuration)
  #:use-module (dwl-guile configuration records)
  #:use-module (dtao-guile home-service)
  #:use-module (engstrand utils)
  #:export (
            feature-laptop
            feature-laptop-tlp
            feature-laptop-natural-scrolling
            feature-laptop-statusbar-battery
            feature-laptop-monitor-brightness

            %engstrand-laptop-base-features))

(define* (feature-laptop)
  "Base laptop feature."

  (feature
   (name 'laptop)
   (values `((laptop . #t)))))

(define* (feature-laptop-tlp
          #:key
          (tlp-config (tlp-configuration
                       (cpu-scaling-governor-on-ac (list "performance"))
                       (sched-powersave-on-bat? #t))))
  "Setup TLP for power management on laptops."

  (define (get-system-services config)
    "Return a list of system services required by TLP"
    (list
     (simple-service
      'add-tlp-system-packages-to-profile
      profile-service-type
      (pkgs '("tlp")))
     (service tlp-service-type
              tlp-config)))

  (feature
   (name 'laptop-tlp)
   (system-services-getter get-system-services)))

(define* (feature-laptop-natural-scrolling
          #:key
          (natural-scrolling? #t))
  "Enable/disable natural scrolling in compositor."

  (ensure-pred boolean? natural-scrolling?)

  (define (get-home-services config)
    (make-service-list
     (when (get-value 'dwl-guile config)
       (simple-service
        'set-natural-scrolling-in-dwl-guile
        home-dwl-guile-service-type
        (modify-dwl-guile-config
         (config =>
                 (dwl-config
                  (inherit config)
                  (natural-scrolling? natural-scrolling?))))))))

  (feature
   (name 'laptop-natural-scrolling)
   (home-services-getter get-home-services)))

(define* (feature-laptop-statusbar-battery
          #:key
          (interface "BAT0"))
  "Add battery indicator to statusbar."

  (ensure-pred string? interface)

  (define read-path
    (string-append "/sys/class/power_supply/"
                   interface
                   "/capacity"))

  (define (get-home-services config)
    (throw-message
     (not (file-exists? read-path))
     (string-append "Invalid battery interface name. No such file " read-path))

    (make-service-list
     (when (get-value 'dtao-guile config)
       (simple-service
        'add-dtao-guile-battery-block
        home-dtao-guile-service-type
        (list
         (dtao-block
          (position "right")
          (interval 10)
          (render
           `(let* ((port (open-input-file ,read-path))
                   (result (read-line port))
                   (percent (string->number result)))
              (close-port port)
              (string-append "^fg("
                             (cond
                              ((<= percent 20) "#ff0000")
                              ((<= percent 50) "#ffcc00")
                              (else "#00ff00"))
                             ")" result "%^fg()")))))))))

  (feature
   (name 'laptop-statusbar-battery)
   (home-services-getter get-home-services)))

(define* (feature-laptop-monitor-brightness
          #:key
          (step 10)
          (decrease-brightness-key "<XF86MonBrightnessDown>")
          (increase-brightness-key "<XF86MonBrightnessUp>")
          (add-keybindings? #t))
  "Install and configure brightnessctl for laptops"

  (ensure-pred number? step)
  (ensure-pred string? decrease-brightness-key)
  (ensure-pred string? increase-brightness-key)
  (ensure-pred boolean? add-keybindings?)

  (define (get-home-services config)
    (make-service-list
     (simple-service
      'add-brightnessctl-home-packages-to-profile
      home-profile-service-type
      (list brightnessctl))
     (when (and add-keybindings?
                (get-value 'dwl-guile config))
       (let ((bin (file-append brightnessctl "/bin/brightnessctl"))
             (change (string-append (number->string step) "%")))
         (simple-service
          'add-dwl-guile-brightness-keys
          home-dwl-guile-service-type
          (modify-dwl-guile-config
           (config =>
                   (dwl-config
                    (inherit config)
                    (keys
                     (append
                      (list
                       (dwl-key
                        (key decrease-brightness-key)
                        (action `(system* ,bin "s" ,(string-append change "-"))))
                       (dwl-key
                        (key increase-brightness-key)
                        (action `(system* ,bin "s" ,(string-append "+" change)))))
                      (dwl-config-keys config)))))))))))

  (feature
   (name 'laptop-monitor-brightness)
   (home-services-getter get-home-services)))

(define %engstrand-laptop-base-features
  (list
   (feature-laptop)
   (feature-laptop-tlp)
   (feature-laptop-natural-scrolling)
   (feature-laptop-statusbar-battery
    #:interface "BAT0")
   (feature-laptop-monitor-brightness)))
