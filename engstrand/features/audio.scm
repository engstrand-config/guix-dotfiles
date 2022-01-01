(define-module (engstrand features audio)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (gnu services)
  #:use-module (gnu packages linux)
  #:use-module (gnu home services)
  #:use-module (engstrand utils)
  #:use-module (engstrand packages wayland)
  #:use-module (dwl-guile home-service)
  #:use-module (dwl-guile configuration)
  #:export (feature-alsa-control))

(define* (feature-alsa-control
          #:key
          (step 5)
          (increase-volume-key "<XF86AudioRaiseVolume>")
          (decrease-volume-key "<XF86AudioLowerVolume>")
          (mute-volume-key "<XF86AudioMute>")
          (add-keybindings? #t))
  "Install and configure alsa-utils and add wm keybindings."

  (ensure-pred number? step)
  (ensure-pred string? increase-volume-key)
  (ensure-pred string? decrease-volume-key)
  (ensure-pred string? mute-volume-key)
  (ensure-pred boolean? add-keybindings?)

  ;; Volume change in percent
  (define change (string-append (number->string step) "%"))
  ;; amixer arguments
  (define args (list (file-append alsa-utils "/bin/amixer")
                     "-D" "pipewire" "sset" "Master"))

  (define (get-home-services config)
    (make-service-list
     (simple-service
      'add-pipewire-control-home-packages-to-profile
      home-profile-service-type
      (list alsa-utils))
     (when (and add-keybindings? (get-value 'dwl-guile config))
       (simple-service
        'add-amixer-dwl-keybindings
        home-dwl-guile-service-type
        (modify-dwl-guile-config
         (config =>
                 (dwl-config
                  (inherit config)
                  (keys
                   (append
                    (list
                     (dwl-key
                      (key increase-volume-key)
                      (action `(system* ,@args
                                        ,(string-append change "+")
                                        "unmute")))
                     (dwl-key
                      (key decrease-volume-key)
                      (action `(system* ,@args
                                        ,(string-append change "-")
                                        "unmute")))
                     (dwl-key
                      (key mute-volume-key)
                      (action `(system* ,@args "toggle"))))
                    (dwl-config-keys config))))))))))

  (feature
   (name 'alsa-control)
   (home-services-getter get-home-services)))
