(define-module (engstrand features audio)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (gnu services)
  #:use-module (gnu packages music)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu home services)
  #:use-module (engstrand utils)
  #:use-module (engstrand packages wayland)
  #:use-module (dwl-guile home-service)
  #:export (feature-pulseaudio-control
            feature-playerctl))

(define* (feature-pulseaudio-control
          #:key
          (step 5)
          (increase-volume-key "<XF86AudioRaiseVolume>")
          (decrease-volume-key "<XF86AudioLowerVolume>")
          (mute-volume-key "<XF86AudioMute>")
          (add-keybindings? #t))
  "Install and configure pamixer and add wm keybindings for Pulseaudio control."

  (ensure-pred number? step)
  (ensure-pred string? increase-volume-key)
  (ensure-pred string? decrease-volume-key)
  (ensure-pred string? mute-volume-key)
  (ensure-pred boolean? add-keybindings?)

  (define command (file-append pamixer "/bin/pamixer"))

  (define (get-home-services config)
    (make-service-list
     (simple-service
      'add-pulseaudio-control-home-packages-to-profile
      home-profile-service-type
      (list pamixer))
     (when (and add-keybindings? (get-value 'dwl-guile config))
       (simple-service
        'add-pamixer-dwl-keybindings
        home-dwl-guile-service-type
        `((set-keys ,increase-volume-key
                    (lambda ()
                      (dwl:shcmd ,command
                                 "--unmute"
                                 "--increase" ,(number->string step)))
                    ,decrease-volume-key
                    (lambda ()
                      (dwl:shcmd ,command
                                 "--unmute"
                                 "--decrease" ,(number->string step)))
                    ,mute-volume-key
                    (lambda () (dwl:shcmd ,command "--toggle-mute"))))))))

  (feature
   (name 'pulseaudio-control)
   (home-services-getter get-home-services)))

(define* (feature-playerctl
          #:key
          (play-key "<XF86AudioPlay>")
          (pause-key "<XF86AudioPause>")
          (next-key "<XF86AudioNext>")
          (previous-key "<XF86AudioPrev>")
          (stop-key "<XF86AudioStop>")
          (add-keybindings? #t))

  (define command (file-append playerctl "/bin/playerctl"))

  (define (get-home-services config)
    (make-service-list
     (simple-service
      'add-playerctl-packages-to-profile
      home-profile-service-type
      (list playerctl))
     (when (and add-keybindings? (get-value 'dwl-guile config))
       (simple-service
        'add-playerctl-dwl-keybindings
        home-dwl-guile-service-type
        ;; Many keyboards (mine included) have a single key for play/pause.
        ;; Consider some additional logic for having separate buttons.
        `((set-keys ,play-key (lambda () (dwl:shcmd ,command "play-pause"))
                    ,pause-key (lambda () (dwl:shcmd ,command "play-pause"))
                    ,next-key (lambda () (dwl:shcmd ,command "next"))
                    ,previous-key (lambda () (dwl:shcmd ,command "previous"))
                    ,stop-key (lambda () (dwl:shcmd ,command "stop"))))))))

  (feature
   (name 'playerctl)
   (home-services-getter get-home-services)))
