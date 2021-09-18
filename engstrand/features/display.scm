(define-module (engstrand features display)
               #:use-module (srfi srfi-1)
               #:use-module (rde features)
               #:use-module (rde features predicates)
               #:use-module (gnu services)
               #:use-module (dwl-guile utils)
               #:use-module (dwl-guile patches)
               #:use-module (dwl-guile home-service)
               #:use-module (dwl-guile configuration)
               #:use-module (dwl-guile configuration records)
               #:use-module (engstrand utils)
               #:export (feature-dwl-guile-monitor-config))

;(define (feature-monitor-brightness)
;  TODO: requires package ddcutil from (gnu packages hardware)
;  please check if package i2c-tools from (gnu packages linux) is required
;  TODO: add kernel module i2c-dev
;  TODO: add user to group i2c-dev
;  TODO: guile bindings for ddcutil?
;  example commands that we have used before:
;  ddcutil setvcp 10 $(echo -e "0\n50\n100" | bemenu -i -p "Set monitor brightness level:")
;  or (increases brightness by 5 %):
;  ddcutil setvcp 10 + 5
;  )

(define (list-of-monitor-rules? x)
  (every dwl-monitor-rule? x))

(define* (feature-dwl-guile-monitor-config
           #:key
           (monitors '())
           (focus-left-key "Left")
           (focus-right-key "Right")
           (focus-modifiers '(SUPER))
           (move-left-key "Left")
           (move-right-key "Right")
           (move-modifiers '(SUPER SHIFT))
           (add-keybindings? #t))
         "Configure monitor settings for dwl-guile."

         (ensure-pred list-of-monitor-rules? monitors)
         (ensure-pred keycode? focus-left-key)
         (ensure-pred keycode? focus-right-key)
         (ensure-pred keycode? move-left-key)
         (ensure-pred keycode? move-right-key)
         (ensure-pred list-of-modifiers? focus-modifiers)
         (ensure-pred list-of-modifiers? move-modifiers)

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
                                       (modifiers focus-modifiers)
                                       (action `(dwl:focus-monitor DIRECTION-LEFT)))
                                     (dwl-key
                                       (key focus-right-key)
                                       (modifiers focus-modifiers)
                                       (action `(dwl:focus-monitor DIRECTION-RIGHT)))
                                     (dwl-key
                                       (key move-left-key)
                                       (modifiers move-modifiers)
                                       (action `(dwl:tag-monitor DIRECTION-LEFT)))
                                     (dwl-key
                                       (key move-right-key)
                                       (modifiers move-modifiers)
                                       (action `(dwl:tag-monitor DIRECTION-RIGHT))))
                                   (dwl-config-keys config)))))))))))

         (feature
           (name 'dwl-guile-monitor-config)
           (home-services-getter get-home-services)))
