(define-module (engstrand features browsers)
               #:use-module (rde features)
               #:use-module (rde features predicates)
               #:use-module (guix gexp)
               #:use-module (gnu services)
               #:use-module (gnu home-services)
               #:use-module (dwl-guile utils)
               #:use-module (dwl-guile home-service)
               #:use-module (dwl-guile configuration)
               #:use-module (gnu packages web-browsers)
               #:use-module (engstrand utils)
               #:export (feature-qutebrowser))

; TODO: Add firefox feature?
;       The problem with installing it from Guix is that it will almost always
;       require you to compile it yourself (which takes a huge amount of time).
;       You can skip the compilation step if you install it from nix.

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
