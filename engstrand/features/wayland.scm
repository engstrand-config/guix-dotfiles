(define-module (engstrand features wayland)
  #:use-module (rde features)
  #:use-module (rde features fontutils)
  #:use-module (rde features predicates)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (gnu services)
  #:use-module (gnu services xorg)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages image)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (engstrand utils)
  #:use-module (engstrand utils bemenu-prompt)
  #:use-module (engstrand systems)
  #:use-module (engstrand packages wayland)
  #:use-module (farg config)
  #:use-module (farg reload)
  #:use-module (farg colorscheme)
  #:use-module (farg home-service)
  #:use-module (dwl-guile utils)
  #:use-module (dwl-guile patches)
  #:use-module (dwl-guile home-service)
  #:export (
            feature-wayland-bemenu
            feature-wayland-bemenu-power
            feature-wayland-foot
            feature-wayland-mako
            feature-wayland-swaybg
            feature-wayland-wlsunset
            feature-wayland-screenshot
            feature-wayland-swaylock))

(define* (feature-wayland-mako
          #:key
          (dismiss-key "C-s-d")
          (dismiss-all-key "C-S-s-d")
          (add-keybindings? #t))
  "Setup mako, a lightweight notification daemon for Wayland"

  (ensure-pred string? dismiss-key)
  (ensure-pred string? dismiss-all-key)
  (ensure-pred boolean? add-keybindings?)

  (lambda (fconfig palette)
    (define (get-home-services config)
      "Return a list of home services required by mako"
      (require-value 'font-monospace config)
      (make-service-list
       (simple-service
        'add-mako-home-packages-to-profile
        home-profile-service-type
        (pkgs '("mako" "libnotify")))
       (simple-service
        'create-mako-config
        home-files-service-type
        `((".config/mako/config"
           ,(alist->ini "mako-config"
                        `(("font"
                           . ,(font->string 'pango 'font-sans config
                                            #:size 11))
                          ("background-color" . ,(palette 'background))
                          ("text-color" . ,(palette 'text))
                          ("width" . 370)
                          ("height" . 100)
                          ("border-color" . ,(offset (palette 'background) 25))
                          ("border-size" . 1)
                          ("border-radius" . 0)
                          ("margin" . 5)
                          ("padding" . 10)
                          ("default-timeout" . 15000)
                          ("anchor" . "top-right")
                          ("max-visible" . 2)
                          ("format" . "<b>%s (%a)</b>\\n%b")
                          ("[grouped=true]")
                          ("format" . "<b>%s (%a, %g)</b>\\n%b")
                          ("[hidden]")
                          ("format" . "(%h more notifications)"))))))
       (simple-service
        'reload-mako-config-on-farg-activation
        home-farg-service-type
        (modify-farg-config
         (config =>
                 (farg-config
                  (inherit config)
                  (activation-commands
                   (cons
                    #~(begin
                        (display "Reloading mako configuration...\n")
                        (system* #$(file-append mako "/bin/makoctl") "reload"))
                    (farg-config-activation-commands config)))))))
       (when (and add-keybindings? (get-value 'dwl-guile config))
         (simple-service
          'add-mako-dwl-keybindings
          home-dwl-guile-service-type
          `((set-keys ,dismiss-key (lambda () (dwl:shcmd ,(file-append mako "/bin/makoctl") "dismiss"))
                      ,dismiss-all-key (lambda () (dwl:shcmd ,(file-append mako "/bin/makoctl")
                                                             "dismiss" "--all"))))))))

    (feature
     (name 'wayland-mako)
     (home-services-getter get-home-services))))

;; TODO: Move to features/terminals.scm?
(define* (feature-wayland-foot
          #:key
          (package foot)
          (open-key "s-<return>")
          (set-default-terminal? #t)
          (swallow-clients? #t))
  "Setup foot terminal."

  (ensure-pred package? package)
  (ensure-pred string? open-key)
  (ensure-pred boolean? set-default-terminal?)
  (ensure-pred boolean? swallow-clients?)

  (lambda (_ palette)
    (define (get-home-services config)
      "Return a list of home services required by foot."
      (require-value 'font-monospace config)

      ;; Terminal color overrides for the default pywal colors.
      (define color-overrides
        `((0 . ,(palette '0-text))
          (1 . ,(palette '1-text))
          (2 . ,(palette '2-text))
          (3 . ,(palette '3-text))
          (4 . ,(palette '4-text))
          (5 . ,(palette '5-text))
          (6 . ,(palette '6-text))
          (7 . ,(palette '7-text))
          (8 . ,(brighten (palette '0-text) 10))
          (9 . ,(brighten (palette '1-text) 10))
          (10 . ,(brighten (palette '2-text) 10))
          (11 . ,(brighten (palette '3-text) 10))
          (12 . ,(brighten (palette '4-text) 10))
          (13 . ,(brighten (palette '5-text) 10))
          (14 . ,(brighten (palette '6-text) 10))
          (15 . ,(brighten (palette '7-text) 10))))

      (let ((has-dwl-guile? (get-value 'dwl-guile config)))
        (make-service-list
         (simple-service
          'add-foot-home-packages-to-profile
          home-profile-service-type
          (list package))
         (simple-service
          'create-foot-config
          home-files-service-type
          `((".config/foot/foot.ini"
             ,(alist->ini "foot-config"
                          `(("pad" . "5x5")
                            ("font" . "monospace:size=12")
                            ("dpi-aware" . "no")
                            ;; nmtui does not like if term is set to foot
                            ("term" . "xterm")

                            ("[colors]")
                            ("alpha" . ,(palette 'alpha))
                            ("foreground" . ,(strip-hex (palette 'text)))
                            ("background" . ,(strip-hex (palette 'background)))
                            ("regular0" . ,(strip-hex (assoc-ref color-overrides 0)))
                            ("regular1" . ,(strip-hex (assoc-ref color-overrides 1)))
                            ("regular2" . ,(strip-hex (assoc-ref color-overrides 2)))
                            ("regular3" . ,(strip-hex (assoc-ref color-overrides 3)))
                            ("regular4" . ,(strip-hex (assoc-ref color-overrides 4)))
                            ("regular5" . ,(strip-hex (assoc-ref color-overrides 5)))
                            ("regular6" . ,(strip-hex (assoc-ref color-overrides 6)))
                            ("regular7" . ,(strip-hex (assoc-ref color-overrides 7)))
                            ("bright0" . ,(strip-hex (assoc-ref color-overrides 8)))
                            ("bright1" . ,(strip-hex (assoc-ref color-overrides 9)))
                            ("bright2" . ,(strip-hex (assoc-ref color-overrides 10)))
                            ("bright3" . ,(strip-hex (assoc-ref color-overrides 11)))
                            ("bright4" . ,(strip-hex (assoc-ref color-overrides 12)))
                            ("bright5" . ,(strip-hex (assoc-ref color-overrides 13)))
                            ("bright6" . ,(strip-hex (assoc-ref color-overrides 14)))
                            ("bright7" . ,(strip-hex (assoc-ref color-overrides 15)))
                            ("dim1" . ,(strip-hex (palette 'red)))
                            ("dim2" . ,(strip-hex (palette 'green)))

                            ("[key-bindings]")
                            ("scrollback-up-line" . "Mod1+k")
                            ("scrollback-down-line" . "Mod1+j")
                            ("clipboard-copy" . "Mod1+c")
                            ("clipboard-paste" . "Mod1+v")
                            ("search-start" . "Mod1+s")
                            ("font-increase" . "Mod1+Control+k")
                            ("font-decrease" . "Mod1+Control+j")
                            ("font-reset" . "Mod1+Control+0")
                            ;; This should be defined in dwl.
                            ("spawn-terminal" . "Mod4+Shift+Return")
                            ("show-urls-launch" . "Mod1+u")
                            ("show-urls-copy" . "Mod1+Control+u")

                            ("[search-bindings]")
                            ("find-prev" . "Mod1+p")
                            ("find-next" . "Mod1+n")
                            ("cursor-left" . "Mod1+h")
                            ("cursor-right" . "Mod1+l")
                            ("cursor-left-word" . "Mod1+b")
                            ("cursor-right-word" . "Mod1+w")
                            ("cursor-home" . "Mod1+i")
                            ("cursor-end" . "Mod1+a")
                            ("clipboard-paste" . "Mod1+v")

                            ("[mouse-bindings]")
                            ("select-begin-block" . "none")
                            ("select-word-whitespace"  . "Mod1+BTN_LEFT-2"))))))
         (simple-service
          'reload-open-foot-instances-on-farg-activation
          home-farg-service-type
          (modify-farg-config
           (config =>
                   (farg-config
                    (inherit config)
                    (activation-commands
                     (cons
                      #~(begin
                          (display "Reloading theme in open foot instances...\n")
                          #$(reload-terminal-colors palette color-overrides))
                      (farg-config-activation-commands config)))))))
         (when has-dwl-guile?
           (simple-service
            'set-as-default-terminal-in-dwl-guile
            home-dwl-guile-service-type
            `((set-keys "s-<return>" (lambda () (dwl:spawn ,(file-append foot "/bin/foot"))))
              (set-rules '((id . "foot")
                           (alpha . ,(palette 'alpha))
                           (terminal? . ,swallow-clients?)))))))))

    (feature
     (name 'wayland-foot)
     (home-services-getter get-home-services))))

(define* (feature-wayland-swaybg
          #:key
          (path #f)
          (auto-start? #t))
  "Setup swaybg for setting wallpaper in Wayland compositors."

  (ensure-pred maybe-string? path)
  (ensure-pred boolean? auto-start?)

  (lambda (fconfig palette)
    (define wallpaper-path
      (let ((colorscheme (home-farg-configuration-colorscheme fconfig)))
        (match path
          (#f (colorscheme-wallpaper colorscheme))
          (else path))))

    (define user-wallpaper-path
      (string-append (getenv "HOME") "/.config/wallpaper.jpg"))

    (define (get-home-services config)
      "Return a list of home services required by swaybg"
      (let ((has-dwl-guile? (get-value 'dwl-guile config)))
        (make-service-list
         (simple-service
          'add-wbg-home-packages-to-profile
          home-profile-service-type
          (list swaybg))
         (when wallpaper-path
           (simple-service
            'copy-wallpaper-to-profile
            home-files-service-type
            `((".config/wallpaper.jpg" ,(local-file wallpaper-path)))))
         (when wallpaper-path
           (simple-service
            'reload-wallpaper-on-farg-activation
            home-farg-service-type
            (modify-farg-config
             (config =>
                     (farg-config
                      (inherit config)
                      (activation-commands
                       (cons
                        #~(begin
                            (display "Reloading swaybg to update wallpaper...\n")
                            (with-output-to-file "/dev/null"
                              (lambda ()
                                (system* #$(file-append shepherd "/bin/herd") "restart" "swaybg"))))
                        (farg-config-activation-commands config))))))))
         (when wallpaper-path
           (simple-service
            'add-swaybg-shepherd-service
            home-shepherd-service-type
            (list
             (shepherd-service
              (documentation "Run swaybg.")
              (provision '(swaybg))
              (requirement (if has-dwl-guile? '(dwl-guile) '()))
              (auto-start? auto-start?)
              (respawn? #t)
              (start
               #~(make-forkexec-constructor
                  (list #$(file-append swaybg "/bin/swaybg")
                        "-i" #$user-wallpaper-path
                        "--mode" "fill")
                  #:log-file #$(make-log-file "swaybg")))
              (stop #~(make-kill-destructor)))))))))

    (feature
     (name 'wayland-swaybg)
     (home-services-getter get-home-services))))

(define* (feature-wayland-wlsunset
          #:key
          (package wlsunset)
          (auto-start? #t)
          (toggle-key "s-<end>")
          (latitude 59.8)
          (longitude 17.6)
          (gamma-low 2000)
          (gamma-high 6500)
          (add-keybindings? #t))
  "Setup wlsunset for adjusting day/night gamma for Wayland compositors."

  (ensure-pred package? wlsunset)
  (ensure-pred boolean? auto-start?)
  (ensure-pred string? toggle-key)
  (ensure-pred number? latitude)
  (ensure-pred number? longitude)
  (ensure-pred number? gamma-low)
  (ensure-pred number? gamma-high)
  (ensure-pred boolean? add-keybindings?)

  (define (get-home-services config)
    "Return a list of home services required by wlsunset"
    (let ((has-dwl-guile? (get-value 'dwl-guile config)))
      (make-service-list
       (simple-service
        'add-wlsunset-home-packages-to-profile
        home-profile-service-type
        (list package))
       (simple-service
        'add-wlsunset-shepherd-service
        home-shepherd-service-type
        (list
         (shepherd-service
          (documentation "Run wlsunset.")
          (provision '(wlsunset))
          (requirement (if has-dwl-guile? '(dwl-guile) '()))
          (auto-start? auto-start?)
          (respawn? #t)
          (start
           #~(make-forkexec-constructor
              (list
               #$(file-append wlsunset "/bin/wlsunset")
               #$(string-append "-l" (number->string latitude))
               #$(string-append "-L" (number->string longitude))
               #$(string-append "-t" (number->string gamma-low))
               #$(string-append "-T" (number->string gamma-high)))
              #:log-file #$(make-log-file "wlsunset")))
          (actions
           (list
            (shepherd-action
             (name 'toggle)
             (documentation "Toggles the wlsunset service on/off.")
             (procedure #~(lambda (running?)
                            (if running?
                                (stop 'wlsunset)
                                (start 'wlsunset))
                            #t)))))
          (stop #~(make-kill-destructor)))))
       (when (and add-keybindings? has-dwl-guile?)
         (simple-service
          'add-wlsunset-dwl-keybindings
          home-dwl-guile-service-type
          `((set-keys ,toggle-key
                      (lambda () (dwl:shcmd ,(file-append shepherd "/bin/herd")
                                            "toggle" "wlsunset")))))))))

  (feature
   (name 'wayland-wlsunset)
   (home-services-getter get-home-services)))

(define* (feature-wayland-screenshot
          #:key
          (output-filetype "png")
          (jpeg-quality 100)
          (png-compression-level 6)
          (include-cursors? #f)
          (screenshot-output-key "S-s-<print>")
          (screenshot-select-key "s-<print>")
          (screenshot-select-copy-key "<print>")
          (add-keybindings? #t))
  "Setup grim, slurp and wl-clipboard for taking screenshots in Wayland compositors."

  (ensure-pred string? output-filetype)
  (ensure-pred number? jpeg-quality)
  (ensure-pred number? png-compression-level)
  (ensure-pred boolean? include-cursors?)
  (ensure-pred string? screenshot-output-key)
  (ensure-pred string? screenshot-select-key)
  (ensure-pred string? screenshot-select-copy-key)
  (ensure-pred boolean? add-keybindings?)

  (define %grim-command
    `(,(file-append grim "/bin/grim")
      ,(if include-cursors? "-c" "")
      "-t" ,output-filetype
      ,@(if (eq? output-filetype "jpeg")
            (list "-q" (number->string jpeg-quality))
            '())
      ,@(if (eq? output-filetype "png")
            (list "-l" (number->string png-compression-level))
            '())))

  (define %grim-select-options
    `("-g" "\"$(" ,(file-append slurp "/bin/slurp" ")\"")))

  (define %grim-pipe-to-clipboard
    `("-" "|" ,(file-append wl-clipboard "/bin/wl-copy")))

  ;; TODO: Cleanup this mess. A simple solution is to just use the executable name directly.
  ;;       Another (better) solution is to allow multiple arguments to dwl:shcmd.
  ;;       dwl:spawn does support n amount of arguments, but since shcmd runs the command
  ;;       in a shell context ("/bin/sh" "-c" <args>), the last argument must be a single string.
  (define (make-screenshot-shcmd . params)
    `(dwl:shcmd (string-join (list ,@(fold-right append '() (cons %grim-command params))))))

  (define (get-home-services config)
    "Return a list of home services required for screenshots."
    (make-service-list
     (simple-service
      'add-screenshot-home-packages-to-profile
      home-profile-service-type
      (pkgs '("grim" "slurp" "wl-clipboard")))
     (when (and add-keybindings? (get-value 'dwl-guile config))
       (simple-service
        'add-screenshot-dwl-keybindings
        home-dwl-guile-service-type
        `((set-keys ,screenshot-output-key
                    (lambda () ,(make-screenshot-shcmd))
                    ,screenshot-select-key
                    (lambda () ,(make-screenshot-shcmd %grim-select-options))
                    ,screenshot-select-copy-key
                    (lambda () ,(make-screenshot-shcmd %grim-select-options
                                                       %grim-pipe-to-clipboard))))))))

  (feature
   (name 'wayland-screenshots)
   (home-services-getter get-home-services)))

(define* (feature-wayland-bemenu
          #:key
          (open-key "s-d")
          (set-default-menu? #t))
  "Setup bemenu."

  (ensure-pred string? open-key)
  (ensure-pred boolean? set-default-menu?)

  (lambda (fconfig palette)
    (define (get-home-services config)
      "Return a list of home services required by bemenu."
      (require-value 'font-monospace config)
      (make-service-list
       (simple-service
        'add-bemenu-home-packages-to-profile
        home-profile-service-type
        (list bemenu))
       (when (and set-default-menu? (get-value 'dwl-guile config))
         (simple-service
          'set-bemenu-as-default-menu-in-dwl-guile
          home-dwl-guile-service-type
          `((set-keys ,open-key (lambda () (dwl:spawn ,(file-append bemenu "/bin/bemenu-run")))))))
       (simple-service
        'bemenu-options
        home-environment-variables-service-type
        (alist->environment-variable
         "BEMENU_OPTS"
         `(("ignorecase" . #t)
           ("line-height"
            . ,(get-value 'statusbar-height config 25))
           ("filter" . #f)
           ("wrap" . #f)
           ("list" . #f)
           ("prompt" . #f)
           ("prefix" . #f)
           ("index" . #f)
           ("password" . #f)
           ("scrollbar" . #f)
           ("ifne" . #f)
           ("fork" . #f)
           ("no-exec" . #f)
           ("bottom" . #f)
           ("grab" . #f)
           ("no-overlap" . #f)
           ("monitor" . #f)
           ("fn"
            . ,(font->string 'pango 'font-monospace config
                             #:bold? #t
                             #:size 10))
           ("tb" . ,(palette 'primary))
           ("tf" . ,(make-readable (palette 'primary)
                                   (palette 'primary)))
           ("fb" . ,(palette 'background))
           ("ff" . ,(palette 'text))
           ("nb" . ,(palette 'background))
           ("nf" . ,(palette 'text))
           ("ab" . ,(palette 'background))
           ("af" . ,(palette 'text))
           ("hb" . ,(offset (palette 'background) 10))
           ("hf" . ,(palette 'primary-text))
           ("sb" . #f)
           ("sf" . ,(palette 'secondary-text))
           ("scb" . #f)
           ("scf" . #f))))))

    (feature
     (name 'wayland-bemenu)
     (home-services-getter get-home-services))))

(define* (feature-wayland-bemenu-power
          #:key
          (open-key "S-s-<backspace>"))
  "Install and configure bemenu power prompt."

  (define actions
    (let ((loginctl (file-append elogind "/bin/loginctl")))
      `(("suspend" . (system* ,loginctl "suspend"))
        ("restart dwl" . (system* ,(file-append shepherd "/bin/herd") "restart" "dwl-guile"))
        ("logout" . (system* ,loginctl "terminate-session"
                             (getenv "XDG_SESSION_ID")))
        ("reboot" . (system* ,loginctl "reboot"))
        ("shutdown" . (system* ,loginctl "poweroff")))))

  (define (get-home-services config)
    (let ((executable
           (compute-bemenu-prompt
            "bemenu.scm"
            "What do you want to do?"
            actions)))
      (make-service-list
       (simple-service
        'create-bemenu-power-executable
        home-files-service-type
        `((".config/bemenu.scm" ,executable)))
       (when (get-value 'dwl-guile config)
         (simple-service
          'add-bemenu-power-dwl-keybinding
          home-dwl-guile-service-type
          `((set-keys ,open-key (lambda () (dwl:shcmd ,executable)))))))))

  (feature
   (name 'wayland-bemenu-power)
   (home-services-getter get-home-services)))

;; TODO: Add options?
(define* (feature-wayland-swaylock
          #:key
          (lock-key "s-x")
          (add-keybindings? #t))
  "Install and configure swaylock."

  (lambda (_ palette)
    (define (get-home-services config)
      (require-value 'font-monospace config)
      (list
       (simple-service
        'create-swaylock-config
        home-files-service-type
        `((".config/swaylock/config"
           ,(alist->ini "swaylock-config"
                        `(("daemonize")
                          ("hide-keyboard-layout")
                          ("line-uses-ring")
                          ("color" . "00000000")
                          ("font"
                           . ,(font->string 'fcft 'font-monospace config
                                            #:bold? #t))
                          ("font-size" . 40)
                          ("indicator-thickness" . 10)
                          ("indicator-radius" . 80)
                          ("key-hl-color" . ,(strip-hex (palette 'primary)))
                          ("bs-hl-color" . ,(strip-hex (palette 'red)))
                          ("inside-color" . "00000000")
                          ("inside-clear-color" . "00000000")
                          ("inside-ver-color" . "00000000")
                          ("inside-wrong-color" . "00000000")
                          ("ring-color" . ,(strip-hex (offset (palette 'background) 20)))
                          ("ring-ver-color" . ,(strip-hex (palette 'green)))
                          ("ring-wrong-color" . ,(strip-hex (palette 'red)))
                          ("ring-clear-color" . ,(strip-hex (palette 'secondary)))
                          ("text-clear-color" . "00000000")
                          ("text-ver-color" . "00000000")
                          ("text-wrong-color" . "00000000")
                          ("separator-color" . "00000000"))))))
       (when (and add-keybindings? (get-value 'dwl-guile config))
         (simple-service
          'add-swaylock-dwl-keybindings
          home-dwl-guile-service-type
          `((set-keys ,lock-key (lambda () (dwl:shcmd "swaylock"))))))))

    (define (get-system-services config)
      (list
       (simple-service
        'add-screen-locker-system-service
        screen-locker-service-type
        (screen-locker-configuration
        "swaylock" (file-append swaylock "/bin/swaylock") #f))))

    (feature
     (name 'wayland-swaylock)
     (home-services-getter get-home-services)
     (system-services-getter get-system-services))))
