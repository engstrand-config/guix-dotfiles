(define-module (engstrand features browsers)
               #:use-module (rde features)
               #:use-module (rde features predicates)
               #:use-module (guix gexp)
               #:use-module (gnu services)
               #:use-module (gnu home services)
               #:use-module (nongnu packages mozilla)
               #:use-module (dwl-guile utils)
               #:use-module (dwl-guile home-service)
               #:use-module (dwl-guile configuration)
               #:use-module (gnu packages web-browsers)
               #:use-module (engstrand packages browsers)
               #:use-module (engstrand utils)
               #:export (
                         feature-qutebrowser
                         feature-firefox))

(define* (feature-firefox
           #:key
           (open-key "w")
           (open-modifiers '(SUPER SHIFT))
           (spawn-parameters '("firefox"))
           (add-keybindings? #t))
         "Setup Firefox."

         (ensure-pred keycode? open-key)
         (ensure-pred list-of-modifiers? open-modifiers)
         (ensure-pred start-parameters? spawn-parameters)
         (ensure-pred boolean? add-keybindings?)

         (define (get-home-services config)
           "Return a list of home services required by Firefox."
           (make-service-list
             (simple-service
               'add-firefox-home-packages-to-profile
               home-profile-service-type
               (list
                 (if (get-value 'wayland config) firefox/wayland firefox)))
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
           (package qutebrowser-with-scripts)
           (open-key "w")
           (open-modifiers '(SUPER SHIFT))
           (add-keybindings? #t))
         "Setup qutebrowser, a keyboard-focused browser with a minimal GUI."

         (ensure-pred package? package)
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
               (list package))
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
                                     (action `(dwl:spawn ,(file-append package "/bin/qutebrowser")))))
                                 (dwl-config-keys config))))))))))

         (feature
           (name 'qutebrowser)
           (home-services-getter get-home-services)))
