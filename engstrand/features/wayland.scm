(define-module (engstrand features wayland)
               #:use-module (rde features)
               #:use-module (rde features predicates)
               #:use-module (guix gexp)
               #:use-module (srfi srfi-1)
               #:use-module (gnu services)
               #:use-module (gnu packages wm)
               #:use-module (gnu packages image)
               #:use-module (gnu packages admin)
               #:use-module (gnu packages xdisorg)
               #:use-module (gnu packages terminals)
               #:use-module (gnu home services)
               #:use-module (gnu home services shepherd)
               #:use-module (engstrand utils)
               #:use-module (engstrand systems)
               #:use-module (engstrand packages wayland)
               #:use-module (dwl-guile utils)
               #:use-module (dwl-guile patches)
               #:use-module (dwl-guile home-service)
               #:use-module (dwl-guile configuration)
               #:export (
                         feature-wayland-dwl-guile
                         feature-wayland-bemenu
                         feature-wayland-foot
                         feature-wayland-mako
                         feature-wayland-wbg
                         feature-wayland-wlsunset
                         feature-wayland-screenshot

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
    (colors
      (dwl-colors
        (root '(0 0 1 1))))))

;; rewrite with match
(define (transform-bemenu-options lst)
  (define (make-cli-argument config-pair)
    (let ((argument (car config-pair)) (value (cdr config-pair)))
      (if (not value) ""
          (string-append "--" argument
                         (cond ((eq? value #t) "")
                               ((string? value) (string-append " " "'" value "'"))
                               ((number? value) (string-append " " (number->string value)))
                               (else (raise "invalid bemenu argument!")))))))
  (string-join (map make-cli-argument lst)))

; Checks if SYMBOL corresponds to a patch that is/will
; be applied to dwl-guile, based on the feature values in CONFIG.
; SYMBOL should be the name of the patch, not including the ".patch" extension.
; I.e. @code{(has-dwl-patch? 'xwayland config)}.
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
           (dismiss-key "d")
           (dismiss-modifiers '(SUPER CTRL))
           (dismiss-all-key "d")
           (dismiss-all-modifiers '(SUPER CTRL SHIFT))
           (add-keybindings? #t))
         "Setup mako, a lightweight notification daemon for Wayland"

         (ensure-pred string? dismiss-key)
         (ensure-pred string? dismiss-all-key)
         (ensure-pred list-of-modifiers? dismiss-modifiers)
         (ensure-pred list-of-modifiers? dismiss-all-modifiers)
         (ensure-pred boolean? add-keybindings?)

         ; TODO: Allow configuration using Guile.

         (define (get-home-services config)
           "Return a list of home services required by mako"
           (make-service-list
             (simple-service
               'add-mako-home-packages-to-profile
               home-profile-service-type
               (pkgs '("mako" "libnotify")))
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
                                     (modifiers dismiss-modifiers)
                                     (key dismiss-key)
                                     (action `(system* ,(file-append mako "/bin/makoctl")
                                                       "dismiss")))
                                   (dwl-key
                                     (modifiers dismiss-all-modifiers)
                                     (key dismiss-all-key)
                                     (action `(system* ,(file-append mako "/bin/makoctl")
                                                       "dismiss" "--all"))))
                                 (dwl-config-keys config))))))))))

         (feature
           (name 'wayland-mako)
           (home-services-getter get-home-services)))

; TODO: Move to features/terminals.scm?
(define* (feature-wayland-foot
           #:key
           (package foot)
           (set-default-terminal? #t)
           (window-alpha 0.9)
           (swallow-clients? #t)) ;; TODO: Active swallow patch automatically if #t?
         "Setup foot terminal."

         (ensure-pred package? package)
         (ensure-pred boolean? set-default-terminal?)
         (ensure-pred number? window-alpha)
         (ensure-pred boolean? swallow-clients?)

         (define (get-home-services config)
           "Return a list of home services required by foot."
           (let ((has-dwl-guile? (get-value 'dwl-guile config)))
             (make-service-list
               (simple-service
                 'add-foot-home-packages-to-profile
                 home-profile-service-type
                 (list package))
               ; TODO: Allow configuration using Guile.
               (simple-service
                 'create-foot-config
                 home-files-service-type
                 `(("config/foot/foot.ini" ,(local-file "../files/foot.ini"))))
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

; TODO: Move to farg?
; TODO: Copy file at PATH to store and restart shepherd service on change
(define* (feature-wayland-wbg
           #:key
           (path #f))
         "Setup wbg for setting wallpaper in Wayland compositors."

         (ensure-pred maybe-string? path)

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
                       (auto-start? #f)
                       (respawn? #f)
                       (start
                         #~(make-forkexec-constructor
                             (list #$(file-append wbg "/bin/wbg") #$path)))
                       (stop #~(make-kill-destructor))))))
               (when has-dwl-guile?
                 (simple-service
                   'start-wbg-on-dwl-guile-startup
                   home-dwl-guile-service-type
                   (modify-dwl-guile
                     (config =>
                             (home-dwl-guile-configuration
                               (inherit config)
                               (startup-commands
                                 (cons
                                   #~(begin
                                       ; TODO: Figure out why the service is disabled after restarting dwl-guile.
                                       ;       A shepherd service will automatically be disabled if it respawns
                                       ;       and exists too frequently during a certain time period.
                                       ;       I do not understand why this would happen though, since both auto-start?
                                       ;       and respawn? has been set to false. I have also noticied that
                                       ;       it usually works if you use start/stop rather than restart on dwl-guile.
                                       (system* #$(file-append shepherd "/bin/herd") "enable" "wbg")
                                       (system* #$(file-append shepherd "/bin/herd") "start" "wbg"))
                                   (home-dwl-guile-configuration-startup-commands config)))))))))))

         (feature
           (name 'wayland-wbg)
           (home-services-getter get-home-services)))

(define* (feature-wayland-wlsunset
           #:key
           (package wlsunset)
           (toggle-key "End")
           (toggle-modifiers '(SUPER))
           (latitude 59.8)
           (longitude 17.6)
           (gamma-low 2000)
           (gamma-high 6500)
           (add-keybindings? #t))
         "Setup wlsunset for adjusting day/night gamma for Wayland compositors."

         (ensure-pred package? wlsunset)
         (ensure-pred keycode? toggle-key)
         (ensure-pred number? latitude)
         (ensure-pred number? longitude)
         (ensure-pred number? gamma-low)
         (ensure-pred number? gamma-high)
         (ensure-pred list-of-modifiers? toggle-modifiers)
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
                     (auto-start? #f)
                     (respawn? #f)
                     (start
                       #~(make-forkexec-constructor
                           (list
                             #$(file-append wlsunset "/bin/wlsunset")
                             #$(string-append "-l" (number->string latitude))
                             #$(string-append "-L" (number->string longitude))
                             #$(string-append "-t" (number->string gamma-low))
                             #$(string-append "-T" (number->string gamma-high)))))
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
                                       (modifiers toggle-modifiers)
                                       (key toggle-key)
                                       (action `(system* ,(file-append shepherd "/bin/herd")
                                                         "toggle"
                                                         "wlsunset"))))
                                   (dwl-config-keys config))))))))
               (when has-dwl-guile?
                 (simple-service
                   'start-wlsunset-on-dwl-guile-startup
                   home-dwl-guile-service-type
                   (modify-dwl-guile
                     (config =>
                             (home-dwl-guile-configuration
                               (inherit config)
                               (startup-commands
                                 (cons
                                   #~(begin
                                       (system* #$(file-append shepherd "/bin/herd") "enable" "wlsunset")
                                       (system* #$(file-append shepherd "/bin/herd") "start" "wlsunset"))
                                   (home-dwl-guile-configuration-startup-commands config)))))))))))

         (feature
           (name 'wayland-wlsunset)
           (home-services-getter get-home-services)))

(define* (feature-wayland-screenshot
           #:key
           (output-filetype "jpeg")
           (output-quality 100)
           (include-cursors? #f)
           (screenshot-output-key "Print")
           (screenshot-output-modifiers '())
           (screenshot-select-key "Print")
           (screenshot-select-modifiers '(SUPER))
           (screenshot-select-copy-key "Print")
           (screenshot-select-copy-modifiers '(SUPER SHIFT))
           (add-keybindings? #t))
         "Setup grim, slurp and wl-clipboard for taking screenshots in Wayland compositors."

         (ensure-pred string? output-filetype)
         (ensure-pred number? output-quality)
         (ensure-pred boolean? include-cursors?)
         (ensure-pred keycode? screenshot-output-key)
         (ensure-pred keycode? screenshot-select-key)
         (ensure-pred keycode? screenshot-select-copy-key)
         (ensure-pred list-of-modifiers? screenshot-output-modifiers)
         (ensure-pred list-of-modifiers? screenshot-select-modifiers)
         (ensure-pred list-of-modifiers? screenshot-select-copy-modifiers)
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

         ; TODO: Cleanup this mess. A simple solution is to just use the executable name directly.
         ;       Another (better) solution is to allow multiple arguments to dwl:shcmd.
         ;       dwl:spawn does support n amount of arguments, but since shcmd runs the command
         ;       in a shell context ("/bin/sh" "-c" <args>), the last argument must be a single string.
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
                                     (modifiers screenshot-output-modifiers)
                                     (key screenshot-output-key)
                                     (action (make-screenshot-shcmd)))
                                   (dwl-key
                                     (modifiers screenshot-select-modifiers)
                                     (key screenshot-select-key)
                                     (action (make-screenshot-shcmd %grim-select-options)))
                                   (dwl-key
                                     (modifiers screenshot-select-copy-modifiers)
                                     (key screenshot-select-copy-key)
                                     (action (make-screenshot-shcmd %grim-select-options
                                                                    %grim-pipe-to-clipboard))))
                                 (dwl-config-keys config))))))))))

         (feature
           (name 'wayland-screenshots)
           (home-services-getter get-home-services)))

(define* (feature-wayland-bemenu
           #:key
           (options '())
           (set-default-menu? #t))
         "Setup bemenu."

         (define (get-home-services config)
           "Return a list of home services required by bemenu."
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

             ; TODO: Convert options list into a configuration
             ;       and automatically transform when enabling feature.
             (simple-service
               'bemenu-options
               home-environment-variables-service-type
               `(("BEMENU_OPTS" . ,(string-append
                                     "\""
                                     (transform-bemenu-options
                                       '(("ignorecase" . #t)
                                         ("line-height" . 21)
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
                                         ("line-height" . 0)
                                         ("fn" . "'JetBrains Mono 10'")
                                         ("tb" . #f)
                                         ("tf" . #f)
                                         ("fb" . #f)
                                         ("ff" . #f)
                                         ("nb" . #f)
                                         ("nf" . #f)
                                         ("hb" . #f)
                                         ("hf" . #f)
                                         ("sb" . #f)
                                         ("sf" . #f)
                                         ("scb" . #f)
                                         ("scf" . #f)))
                                     "\""))))))

         (feature
           (name 'wayland-bemenu)
           (home-services-getter get-home-services)))
