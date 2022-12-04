(define-module (engstrand features browsers)
  #:use-module (srfi srfi-1)
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
  #:use-module (farg config)
  #:use-module (farg colorscheme)
  #:use-module (farg home-service)
  #:use-module (engstrand packages browsers)
  #:use-module (engstrand utils)
  #:export (
            feature-qutebrowser
            feature-firefox))

(define* (serialize-qutebrowser-config alist)
  "Serializes an alist of qutebrowser options into a config.py"
  (fold
   (lambda (entry acc)
     (let* ((value (cdr entry))
            (str-value (if (number? value)
                           (number->string value)
                           value)))
       (string-append acc "c." (car entry) " = " str-value "\n")))
   "config.load_autoconfig()\n"
   alist))

(define* (feature-firefox
          #:key
          (open-key "S-s-w")
          (spawn-parameters '("firefox"))
          (default-browser? #f))
  "Setup Firefox."

  (ensure-pred string? open-key)
  (ensure-pred start-parameters? spawn-parameters)
  (ensure-pred boolean? default-browser?)

  (define (get-home-services config)
    "Return a list of home services required by Firefox."
    (let ((package (if (get-value 'wayland config) firefox/wayland-95.0.2 firefox)))
      (make-service-list
       (if default-browser?
           (simple-service
            'set-firefox-environment-variable
            home-environment-variables-service-type
            `(("BROWSER" . ,(file-append package "/bin/firefox")))))
       (simple-service
        'add-firefox-home-packages-to-profile
        home-profile-service-type
        (list package))
       (when (and default-browser? (get-value 'dwl-guile config))
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
                        (key open-key)
                        (action `(dwl:spawn ,spawn-parameters))))
                      (dwl-config-keys config)))))))))))

  (feature
   (name 'firefox)
   (home-services-getter get-home-services)))

;; TODO: Add option for custom config
(define* (feature-qutebrowser
          #:key
          (package qutebrowser-with-scripts)
          (open-key "S-s-w")
          (default-browser? #f))
  "Setup qutebrowser, a keyboard-focused browser with a minimal GUI."

  (ensure-pred package? package)
  (ensure-pred string? open-key)
  (ensure-pred boolean? default-browser?)

  (lambda (fconfig palette)
    (define (get-home-services config)
      "Return a list of home services required by qutebrowser"
      (make-service-list
       (if default-browser?
           (simple-service
            'set-qutebrowser-environment-variable
            home-environment-variables-service-type
            `(("BROWSER" . ,(file-append package "/bin/qutebrowser")))))
       (simple-service
        'add-qutebrowser-home-packages-to-profile
        home-profile-service-type
        (list package))
       (simple-service
        'add-qutebrowser-config
        home-files-service-type
        `((".config/qutebrowser/config.py"
           ,(plain-file
             "qutebrowser-config.py"
             (let ((light? (farg-config-light? (home-farg-configuration-config fconfig)))
                   (cursor (str-escape (palette 'text)))
                   (background (str-escape (palette 'background)))
                   (background-offset (str-escape (offset (palette 'background) 10)))
                   (foreground (str-escape (palette 'text)))
                   (primary (str-escape (palette 'primary)))
                   (primary-text (str-escape (palette 'primary-text)))
                   (primary-overlay-text (str-escape (make-readable
                                                      (palette 'primary)
                                                      (palette 'primary))))
                   (secondary-text (str-escape (palette 'secondary-text)))
                   (disabled-text (str-escape (offset (palette 'background) 50)))
                   (red (str-escape (palette 'red)))
                   (green (str-escape (palette 'green)))
                   (blue (str-escape (palette 'blue)))
                   (yellow (str-escape (palette 'yellow)))
                   (red-text (str-escape (palette 'red-text)))
                   (green-text (str-escape (palette 'green-text)))
                   (blue-text (str-escape (palette 'blue-text)))
                   (yellow-text (str-escape (palette 'yellow-text)))
                   (magenta (str-escape (palette 5)))
                   (cyan (str-escape (palette 6)))
                   (none (str-escape "none")))
               (serialize-qutebrowser-config
                `(("auto_save.session" . "True")
                  ("content.blocking.enabled" . "True")
                  ("tabs.position" . ,(str-escape "top"))
                  ("tabs.padding" . ,"{ \"bottom\": 5, \"top\": 5, \"left\": 5, \"right\": 5}")
                  ("tabs.favicons.scale" . 1.0)
                  ("tabs.indicator.width" . 0)
                  ("downloads.position" . ,(str-escape "bottom"))
                  ("downloads.remove_finished" . ,(* 1000 5))
                  ("colors.completion.category.bg" . ,background)
                  ("colors.completion.category.border.bottom" . ,background)
                  ("colors.completion.category.border.top" . ,background)
                  ("colors.completion.category.fg" . ,foreground)
                  ("colors.completion.even.bg" . ,background)
                  ("colors.completion.odd.bg" . ,background)
                  ("colors.completion.fg" . ,foreground)
                  ("colors.completion.item.selected.bg" . ,background-offset)
                  ("colors.completion.item.selected.border.bottom" . ,background)
                  ("colors.completion.item.selected.border.top" . ,background)
                  ("colors.completion.item.selected.fg" . ,foreground)
                  ("colors.completion.item.selected.match.fg" . ,secondary-text)
                  ("colors.completion.match.fg" . ,primary-text)
                  ("colors.completion.scrollbar.bg" . ,background)
                  ("colors.completion.scrollbar.fg" . ,foreground)
                  ("colors.downloads.bar.bg" . ,background)
                  ("colors.downloads.error.bg" . ,red)
                  ("colors.downloads.error.fg" . ,red-text)
                  ("colors.downloads.stop.bg" . ,cyan)
                  ("colors.downloads.system.bg" . ,none)
                  ("colors.hints.bg" . ,primary)
                  ("colors.hints.fg" . ,primary-overlay-text)
                  ("colors.hints.match.fg" . ,blue)
                  ("colors.keyhint.bg" . ,background)
                  ("colors.keyhint.fg" . ,foreground)
                  ("colors.keyhint.suffix.fg" . ,yellow)
                  ("colors.messages.error.bg" . ,red)
                  ("colors.messages.error.border" . ,red)
                  ("colors.messages.error.fg" . ,red-text)
                  ("colors.messages.info.bg" . ,blue)
                  ("colors.messages.info.border" . ,blue)
                  ("colors.messages.info.fg" . ,blue-text)
                  ("colors.messages.warning.bg" . ,yellow)
                  ("colors.messages.warning.border" . ,yellow)
                  ("colors.messages.warning.fg" . ,yellow-text)
                  ("colors.prompts.bg" . ,background)
                  ("colors.prompts.border" . ,none)
                  ("colors.prompts.fg" . ,foreground)
                  ("colors.prompts.selected.bg" . ,magenta)
                  ("colors.statusbar.caret.bg" . ,cyan)
                  ("colors.statusbar.caret.fg" . ,cursor)
                  ("colors.statusbar.caret.selection.bg" . ,cyan)
                  ("colors.statusbar.caret.selection.fg" . ,foreground)
                  ("colors.statusbar.command.bg" . ,background)
                  ("colors.statusbar.command.fg" . ,foreground)
                  ("colors.statusbar.command.private.bg" . ,background)
                  ("colors.statusbar.command.private.fg" . ,foreground)
                  ("colors.statusbar.insert.bg" . ,green)
                  ("colors.statusbar.insert.fg" . ,background)
                  ("colors.statusbar.normal.bg" . ,background)
                  ("colors.statusbar.normal.fg" . ,foreground)
                  ("colors.statusbar.passthrough.bg" . ,blue)
                  ("colors.statusbar.passthrough.fg" . ,blue-text)
                  ("colors.statusbar.private.bg" . ,background)
                  ("colors.statusbar.private.fg" . ,foreground)
                  ("colors.statusbar.progress.bg" . ,foreground)
                  ("colors.statusbar.url.error.fg" . ,red)
                  ("colors.statusbar.url.fg" . ,foreground)
                  ("colors.statusbar.url.hover.fg" . ,blue)
                  ("colors.statusbar.url.success.http.fg" . ,foreground)
                  ("colors.statusbar.url.success.https.fg" . ,green)
                  ("colors.statusbar.url.warn.fg" . ,red)
                  ("colors.tabs.bar.bg" . ,background-offset)
                  ("colors.tabs.even.bg" . ,background-offset)
                  ("colors.tabs.even.fg" . ,foreground)
                  ("colors.tabs.indicator.error" . ,red)
                  ("colors.tabs.indicator.system" . ,none)
                  ("colors.tabs.odd.bg" . ,background-offset)
                  ("colors.tabs.odd.fg" . ,foreground)
                  ("colors.tabs.selected.even.bg" . ,background)
                  ("colors.tabs.selected.even.fg" . ,foreground)
                  ("colors.tabs.selected.odd.bg" . ,background)
                  ("colors.tabs.selected.odd.fg" . ,foreground)
                  ("colors.contextmenu.disabled.bg" . ,background-offset)
                  ("colors.contextmenu.disabled.fg" . ,disabled-text)
                  ("colors.contextmenu.menu.bg" . ,background)
                  ("colors.contextmenu.menu.fg" . ,foreground)
                  ("colors.contextmenu.selected.bg" . ,primary)
                  ("colors.contextmenu.selected.fg" . ,primary-overlay-text)
                  ("colors.webpage.bg" . ,background)
                  ("colors.webpage.preferred_color_scheme"
                   . ,(if light?
                          (str-escape "light")
                          (str-escape "dark"))))))))))
       (simple-service
        'reload-qutebrowser-on-farg-activation
        home-farg-service-type
        (modify-farg-config
         (config =>
                 (farg-config
                  (inherit config)
                  (activation-commands
                   (cons
                    #~(begin
                        (use-modules (ice-9 popen))
                        (display "Reloading qutebrowser to update theme...\n")
                        (let* ((port (open-input-pipe "pgrep qutebrowser"))
                               (pid (read-line port)))
                          (close-port port)
                          (unless (eof-object? pid)
                            (system* #$(file-append package "/bin/qutebrowser")
                                     ":config-source"
                                     ;; Only output potential errors
                                     "-l=critical"))))
                    (farg-config-activation-commands config)))))))
       (when (and default-browser? (get-value 'dwl-guile config))
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
                        (key open-key)
                        (action `(dwl:spawn ,(file-append package "/bin/qutebrowser")
                                            "--qt-arg" "no-sandbox" "true"))))
                      (dwl-config-keys config))))))))))

    (feature
     (name 'qutebrowser)
     (home-services-getter get-home-services))))
