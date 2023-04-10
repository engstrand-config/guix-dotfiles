(define-module (engstrand features bluetooth)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (gnu services)
  #:use-module (dwl-guile home-service)
  #:use-module (engstrand utils)
  #:export (feature-bluetooth-quick-connect))

(define* (feature-bluetooth-quick-connect
          #:key
          (key "S-s-b"))
  "Add keybinding for quickly connecting to bluetooth devices."

  (define (get-home-services config)
    (list
     (when (get-value 'dwl-guile config)
       (simple-service
        'bluetooth-dwl-guile-add-keybindings
        home-dwl-guile-service-type
        `((set-keys ,key
                    (lambda ()
                      ;; emacsclient prevents bluetooth from powering off.
                      (dwl:spawn "emacsclient" "-c"
                                 "--eval" "(bluetooth-list-devices)"))))))))

  (feature
   (name 'bluetooth-quick-connect)
   (home-services-getter get-home-services)))
