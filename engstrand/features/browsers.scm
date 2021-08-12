(define-module (engstrand features browsers)
               #:use-module (rde features)
               #:use-module (rde features predicates)
               #:use-module (guix gexp)
               #:use-module (gnu services)
               #:use-module (gnu home-services)
               #:use-module (nongnu packages mozilla)
               #:use-module (dwl-guile utils)
               #:use-module (dwl-guile home-service)
               #:use-module (dwl-guile configuration)
               #:use-module (gnu packages web-browsers)
               #:use-module (engstrand utils)
               #:export (
                         feature-qutebrowser
                         feature-firefox))

; TODO: Complete this feature. Currently, it is only used to add a dwl keybinding.
(define* (feature-firefox
           #:key
           (open-key "w")
           (open-modifiers '(SUPER SHIFT))
           (spawn-parameters '("firefox"))
           ; (spawn-parameters `(,(file-append firefox "/bin/firefox")))
           (add-keybindings? #t))
         "Setup Firefox."

         (ensure-pred keycode? open-key)
         (ensure-pred list-of-modifiers? open-modifiers)
         (ensure-pred start-parameters? spawn-parameters)
         (ensure-pred boolean? add-keybindings?)

         (define (get-home-services config)
           "Return a list of home services required by Firefox."
           (make-service-list
             ; How do we want to install Firefox?
             ; Installing it from nongnu will require you to manually compile it,
             ; which takes a huge amount of time.
             ; (simple-service
             ;   'add-firefox-home-packages-to-profile
             ;   home-profile-service-type
             ;   (list firefox))
             (when (and add-keybindings? (get-value 'dwl-guile config))
               (simple-service
                 'add-firefox-dwl-keybindings
                 home-dwl-guile-service-type
                 (modify-dwl-guile-config
                   (config =>
                           (dwl-config
                             (inherit config)
                             (keys
                               (append
                                 (list
                                   (dwl-key
                                     (modifiers open-modifiers)
                                     (key open-key)
                                     (action `(dwl:spawn ,spawn-parameters))))
                                 (dwl-config-keys config))))))))))

         (feature
           (name 'firefox)
           (home-services-getter get-home-services)))

(define* (feature-qutebrowser
           #:key
           (open-key "w")
           (open-modifiers '(SUPER SHIFT))
           (add-keybindings? #t))
         "Setup qutebrowser, a keyboard-focused browser with a minimal GUI."

         (ensure-pred keycode? open-key)
         (ensure-pred list-of-modifiers? open-modifiers)
         (ensure-pred boolean? add-keybindings?)
         ; TODO: Add configuration in Guile

         (define (get-home-services config)
           "Return a list of home services required by qutebrowser"
           (make-service-list
             (simple-service
               'add-qutebrowser-home-packages-to-profile
               home-profile-service-type
               (list qutebrowser))
             (when (and add-keybindings? (get-value 'dwl-guile config))
               (simple-service
                 'add-qutebrowser-dwl-keybindings
                 home-dwl-guile-service-type
                 (modify-dwl-guile-config
                   (config =>
                           (dwl-config
                             (inherit config)
                             (keys
                               (append
                                 (list
                                   (dwl-key
                                     (modifiers open-modifiers)
                                     (key open-key)
                                     (action `(dwl:spawn ,(file-append qutebrowser "/bin/qutebrowser")))))
                                 (dwl-config-keys config))))))))))

         (feature
           (name 'qutebrowser)
           (home-services-getter get-home-services)))
