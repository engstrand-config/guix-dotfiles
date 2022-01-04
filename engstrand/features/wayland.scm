(define-module (engstrand features wayland)
  #:use-module (rde features)
  #:use-module (rde features fontutils)
  #:use-module (rde features predicates)
  #:use-module (guix gexp)
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
  #:use-module (dwl-guile utils)
  #:use-module (dwl-guile patches)
  #:use-module (dwl-guile home-service)
  #:use-module (dwl-guile configuration)
  #:use-module (dwl-guile configuration default-config)
  #:export (
            feature-wayland-dwl-guile
            feature-wayland-bemenu
            feature-wayland-bemenu-power
            feature-wayland-foot
            feature-wayland-mako
            feature-wayland-wbg
            feature-wayland-wlsunset
            feature-wayland-screenshot
            feature-wayland-swaylock

            %engstrand-dwl-guile-patches
            %engstrand-dwl-guile-config))

(define %engstrand-dwl-guile-patches
  (list %patch-xwayland
        %patch-swallow
        %patch-movestack
        %patch-attachabove))

(define %engstrand-dwl-guile-config
  (dwl-config
   (xkb-rules %engstrand-keyboard-layout)
   (border-px 2)
   (rules
    (list
     (dwl-rule (id "emacs")
               (title "emacs")
               (alpha 0.9))))
   (keys
    (append
     (list
      (dwl-key
       (key "s-<kp-up>")
       (action '(dwl:increase-masters +1)))
      (dwl-key
       (key "s-<kp-down>")
       (action '(dwl:increase-masters -1)))
      (dwl-key
       (key "s-0")
       (action '(dwl:cycle-layout)))
      (dwl-key
       (key "s-<tab>")
       (action '(dwl:view-previous))))
     %dwl-base-keys))
   (colors
    (dwl-colors
     (root '(0.1 0.1 0.1 1))
     (border '(0.5 0.5 0.5 1))
     (focus '(1 0.8 0 1))))))

;; Checks if SYMBOL corresponds to a patch that is/will
;; be applied to dwl-guile, based on the feature values in CONFIG.
;; SYMBOL should be the name of the patch, not including the ".patch" extension.
;; I.e. @code{(has-dwl-patch? 'xwayland config)}.
(define (has-dwl-patch? symbol config)
  (let ((patch-name (string-append (symbol->string symbol) ".patch")))
    (find (lambda (p) (equal? patch-name (local-file-name p)))
          (get-value 'dwl-guile-patches config))))

(define* (feature-wayland-dwl-guile
          #:key
          (dwl-guile-configuration (home-dwl-guile-configuration)))
  "Setup dwl-guile."

  (ensure-pred home-dwl-guile-configuration? dwl-guile-configuration)

  (define (get-home-services config)
    "Return a list of home services required by dwl."
    (list
     (service home-dwl-guile-service-type
              dwl-guile-configuration)))

  (feature
   (name 'wayland-dwl-guile)
   (values `((wayland . #t)
             (dwl-guile . #t)
             (dwl-guile-patches
              . ,(home-dwl-guile-configuration-patches dwl-guile-configuration))))
   (home-services-getter get-home-services)))

(define* (feature-wayland-mako
          #:key
          (dismiss-key "C-s-d")
          (dismiss-all-key "C-S-s-d")
          (add-keybindings? #t))
  "Setup mako, a lightweight notification daemon for Wayland"

  (ensure-pred string? dismiss-key)
  (ensure-pred string? dismiss-all-key)
  (ensure-pred boolean? add-keybindings?)

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
      `(("config/mako/config"
         ,(alist->ini "mako-config"
                      `(("font"
                         . ,(font->string 'pango 'font-sans config))
                        ("background-color" . "#252525FF")
                        ("text-color" . "#FFFFFFFF")
                        ("width" . 500)
                        ("height" . 85)
                        ("border-color" . "#555555FF")
                        ("border-size" . 1)
                        ("border-radius" . 0)
                        ("margin" . "0,0,10")
                        ("padding" . 10)
                        ("default-timeout" . 15000)
                        ("anchor" . "bottom-center")
                        ("max-visible" . 2)
                        ("format" . "<b>%s (%a)</b>\\n%b")
                        ("[grouped=true]")
                        ("format" . "<b>%s (%a, %g)</b>\\n%b")
                        ("[hidden]")
                        ("format" . "(%h more notifications)"))))))
     (when (and add-keybindings? (get-value 'dwl-guile config))
       (simple-service
        'add-mako-dwl-keybindings
        home-dwl-guile-service-type
        (modify-dwl-guile-config
         (config =>
                 (dwl-config
                  (inherit config)
                  (keys
                   (append
                    (list
                     (dwl-key
                      (key dismiss-key)
                      (action `(system* ,(file-append mako "/bin/makoctl")
                                        "dismiss")))
                     (dwl-key
                      (key dismiss-all-key)
                      (action `(system* ,(file-append mako "/bin/makoctl")
                                        "dismiss" "--all"))))
                    (dwl-config-keys config))))))))))

  (feature
   (name 'wayland-mako)
   (home-services-getter get-home-services)))

;; TODO: Move to features/terminals.scm?
(define* (feature-wayland-foot
          #:key
          (package foot)
          (set-default-terminal? #t)
          (window-alpha 0.9)
          (swallow-clients? #t)) ;; TODO: Add swallow patch automatically if #t?
  "Setup foot terminal."

  (ensure-pred package? package)
  (ensure-pred boolean? set-default-terminal?)
  (ensure-pred number? window-alpha)
  (ensure-pred boolean? swallow-clients?)

  (define (get-home-services config)
    "Return a list of home services required by foot."
    (require-value 'font-monospace config)
    (let ((has-dwl-guile? (get-value 'dwl-guile config)))
      (make-service-list
       (simple-service
        'add-foot-home-packages-to-profile
        home-profile-service-type
        (list package))
       (simple-service
        'create-foot-config
        home-files-service-type
        `(("config/foot/foot.ini"
           ,(alist->ini "foot-config"
                       `(("pad" . "5x5")
                         ;; TODO: I prefer the look of the "monospace" font in foot
                         ;; (whatever the fuck that is). No idea what font is used
                         ;; when monospace is chosen. fc-match shows "DejaVu Sans Mono" "Book",
                         ;; but it does not look like the font used in foot when applied directly.
                         ("font" . "monospace:size=12")
                         ("dpi-aware" . "no")
                         ;; nmtui does not like if term is set to foot
                         ("term" . "xterm")

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
       (when (and set-default-terminal? has-dwl-guile?)
         (simple-service
          'set-foot-as-default-terminal
          home-dwl-guile-service-type
          (modify-dwl-guile-config
           (config =>
                   (dwl-config
                    (inherit config)
                    (terminal `(,(file-append package "/bin/foot"))))))))
       (when has-dwl-guile?
         (simple-service
          'set-foot-window-rule
          home-dwl-guile-service-type
          (modify-dwl-guile-config
           (config =>
                   (dwl-config
                    (inherit config)
                    (rules
                     (append
                      (list
                       (dwl-rule
                        (id "foot")
                        (alpha window-alpha)
                        (no-swallow (not swallow-clients?))
                        (terminal swallow-clients?)))
                      (dwl-config-rules config)))))))))))

  (feature
   (name 'wayland-foot)
   (home-services-getter get-home-services)))

;; TODO: Move to farg?
;; TODO: Copy file at PATH to store and restart shepherd service on change
(define* (feature-wayland-wbg
          #:key
          (path #f)
          (auto-start? #t))
  "Setup wbg for setting wallpaper in Wayland compositors."

  (ensure-pred maybe-string? path)
  (ensure-pred boolean? auto-start?)

  (define (get-home-services config)
    "Return a list of home services required by wbg"
    (let ((has-dwl-guile? (get-value 'dwl-guile config)))
      (make-service-list
       (simple-service
        'add-wbg-home-packages-to-profile
        home-profile-service-type
        (list wbg))
       (when path
         (simple-service
          'add-wbg-shepherd-service
          home-shepherd-service-type
          (list
           (shepherd-service
            (documentation "Run wbg.")
            (provision '(wbg))
            (requirement (if has-dwl-guile? '(dwl-guile) '()))
            (auto-start? auto-start?)
            (respawn? #t)
            (start
             #~(make-forkexec-constructor
                (list #$(file-append wbg "/bin/wbg") #$path)
                #:log-file #$(make-log-file "wbg")))
            (stop #~(make-kill-destructor)))))))))

  (feature
   (name 'wayland-wbg)
   (home-services-getter get-home-services)))

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
          (modify-dwl-guile-config
           (config =>
                   (dwl-config
                    (inherit config)
                    (keys
                     (append
                      (list
                       (dwl-key
                        (key toggle-key)
                        (action `(system* ,(file-append shepherd "/bin/herd")
                                          "toggle"
                                          "wlsunset"))))
                      (dwl-config-keys config)))))))))))

  (feature
   (name 'wayland-wlsunset)
   (home-services-getter get-home-services)))

(define* (feature-wayland-screenshot
          #:key
          (output-filetype "jpeg")
          (output-quality 100)
          (include-cursors? #f)
          (screenshot-output-key "<print>")
          (screenshot-select-key "s-<print>")
          (screenshot-select-copy-key "S-s-<print>")
          (add-keybindings? #t))
  "Setup grim, slurp and wl-clipboard for taking screenshots in Wayland compositors."

  (ensure-pred string? output-filetype)
  (ensure-pred number? output-quality)
  (ensure-pred boolean? include-cursors?)
  (ensure-pred string? screenshot-output-key)
  (ensure-pred string? screenshot-select-key)
  (ensure-pred string? screenshot-select-copy-key)
  (ensure-pred boolean? add-keybindings?)

  (define %grim-command
    `(,(file-append grim "/bin/grim")
      ,(if include-cursors? "-c" "")
      "-t" ,output-filetype
      "-q" ,(number->string output-quality)))

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
        (modify-dwl-guile-config
         (config =>
                 (dwl-config
                  (inherit config)
                  (keys
                   (append
                    (list
                     (dwl-key
                      (key screenshot-output-key)
                      (action (make-screenshot-shcmd)))
                     (dwl-key
                      (key screenshot-select-key)
                      (action (make-screenshot-shcmd %grim-select-options)))
                     (dwl-key
                      (key screenshot-select-copy-key)
                      (action (make-screenshot-shcmd %grim-select-options
                                                     %grim-pipe-to-clipboard))))
                    (dwl-config-keys config))))))))))

  (feature
   (name 'wayland-screenshots)
   (home-services-getter get-home-services)))

(define* (feature-wayland-bemenu
          #:key
          (set-default-menu? #t))
  "Setup bemenu."

  (ensure-pred boolean? set-default-menu?)

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
        'set-bemenu-as-default-menu
        home-dwl-guile-service-type
        (modify-dwl-guile-config
         (config =>
                 (dwl-config
                  (inherit config)
                  (menu `(,(file-append bemenu "/bin/bemenu-run"))))))))

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
         ("tb" . "#FFCC00")
         ("tf" . "#000000")
         ("fb" . "#1A1A1A")
         ("ff" . #f)
         ("nb" . "#1A1A1A")
         ("nf" . "#FFFFFF")
         ("hb" . "#1A1A1A")
         ("hf" . "#FFCC00")
         ("sb" . #f)
         ("sf" . #f)
         ("scb" . #f)
         ("scf" . #f))))))

  (feature
   (name 'wayland-bemenu)
   (home-services-getter get-home-services)))

(define* (feature-wayland-bemenu-power
          #:key
          (open-key "S-s-<backspace>"))
  "Install and configure bemenu power prompt."

  (define actions
    (let ((bin (file-append elogind "/bin/loginctl")))
      `(("suspend" . (system* ,bin "suspend"))
        ("logout" . (system* ,bin "terminate-session"
                             (getenv "XDG_SESSION_ID")))
        ("reboot" . (system* ,bin "reboot"))
        ("shutdown" . (system* ,bin "poweroff")))))

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
        `(("config/bemenu.scm" ,executable)))
       (when (get-value 'dwl-guile config)
         (simple-service
          'add-bemenu-power-dwl-keybinding
          home-dwl-guile-service-type
          (modify-dwl-guile-config
           (config =>
                   (dwl-config
                    (inherit config)
                    (keys
                     (append
                      (list
                       (dwl-key
                        (key open-key)
                        (action `(dwl:shcmd ,executable))))
                      (dwl-config-keys config)))))))))))

  (feature
   (name 'wayland-bemenu-power)
   (home-services-getter get-home-services)))

;; TODO: Add options?
(define* (feature-wayland-swaylock)
  "Install and configure swaylock."

  (define (get-home-services config)
    (require-value 'font-monospace config)
    (list
     (simple-service
      'create-swaylock-config
      home-files-service-type
      `(("config/swaylock/config"
         ,(alist->ini "swaylock-config"
                      `(("clock")
                        ("screenshots")
                        ("indicator")
                        ("daemonize")
                        ("hide-keyboard-layout")
                        ("color" . "000000AA")
                        ("font"
                         . ,(font->string 'fcft 'font-monospace config
                                          #:bold? #t))
                        ("font-size" . 40)
                        ("indicator-thickness" . 8)
                        ("indicator-radius" . 125)
                        ("key-hl-color" . "FF8800")
                        ("key-hl-color" . "FF8800")
                        ("inside-color" . "00000000")
                        ("inside-clear-color" . "00000000")
                        ("inside-ver-color" . "00000000")
                        ("inside-wrong-color" . "00000000")
                        ("ring-color" . "FFCC00")
                        ("ring-wrong-color" . "FF0000")
                        ("text-clear-color" . "00000000")
                        ("text-ver-color" . "00000000")
                        ("text-wrong-color" . "00000000")
                        ("separator-color" . "00000000")
                        ("effect-blur" . "5x5")
                        ("fade-in" . 0)
                        ("datestr" . ""))))))))

  (define (get-system-services config)
    (list
     (screen-locker-service swaylock-effects "swaylock")))

  (feature
   (name 'wayland-swaylock)
   (home-services-getter get-home-services)
   (system-services-getter get-system-services)))
