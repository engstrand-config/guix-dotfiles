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

(define (serialize-qutebrowser-value value)
  (cond
   ((symbol? value) (symbol->string value))
   ((number? value) (number->string value))
   ((string? value) (str-escape value))
   (else value)))

(define* (serialize-qutebrowser-config alist)
  "Serializes an alist of qutebrowser options into a config.py"
  (fold
   (lambda (entry acc)
     (let ((str-value (serialize-qutebrowser-value (cdr entry))))
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
             (let* ((light? (farg-config-light? (home-farg-configuration-config fconfig)))
                    (text (palette 'text))
                    (cursor text)
                    (background (palette 'background))
                    (background-offset (offset background 10))
                    (primary (palette 'primary))
                    (primary-text (palette 'primary-text))
                    (primary-overlay-text (make-readable primary primary))
                    (secondary-text (palette 'secondary-text))
                    (disabled-text (make-readable background-offset background-offset))
                    (red (palette 'red))
                    (green (palette 'green))
                    (blue (palette 'blue))
                    (yellow (palette 'yellow))
                    (red-text (palette 'red-text))
                    (green-text (palette 'green-text))
                    (blue-text (palette 'blue-text))
                    (yellow-text (palette 'yellow-text))
                    (magenta (palette 5))
                    (cyan (palette 6)))
               (serialize-qutebrowser-config
                `(("auto_save.session" . True)
                  ("content.blocking.enabled" . True)
                  ("tabs.position" . ,"top")
                  ("tabs.padding"
                   . ,(string->symbol
                       "{ \"bottom\": 5, \"top\": 5, \"left\": 5, \"right\": 5}"))
                  ("tabs.favicons.scale" . 1.0)
                  ("tabs.indicator.width" . 0)
                  ("downloads.position" . "bottom")
                  ("downloads.remove_finished" . ,(* 1000 5))
                  ("colors.completion.category.bg" . ,background)
                  ("colors.completion.category.border.bottom" . ,background)
                  ("colors.completion.category.border.top" . ,background)
                  ("colors.completion.category.fg" . ,text)
                  ("colors.completion.even.bg" . ,background)
                  ("colors.completion.odd.bg" . ,background)
                  ("colors.completion.fg" . ,text)
                  ("colors.completion.item.selected.bg" . ,background-offset)
                  ("colors.completion.item.selected.border.bottom" . ,background)
                  ("colors.completion.item.selected.border.top" . ,background)
                  ("colors.completion.item.selected.fg" . ,text)
                  ("colors.completion.item.selected.match.fg" . ,secondary-text)
                  ("colors.completion.match.fg" . ,primary-text)
                  ("colors.completion.scrollbar.bg" . ,background)
                  ("colors.completion.scrollbar.fg" . ,text)
                  ("colors.downloads.bar.bg" . ,background)
                  ("colors.downloads.error.bg" . ,red)
                  ("colors.downloads.error.fg" . ,red-text)
                  ("colors.downloads.stop.bg" . ,cyan)
                  ("colors.downloads.system.bg" . "none")
                  ("colors.hints.bg" . ,primary)
                  ("colors.hints.fg" . ,primary-overlay-text)
                  ("colors.hints.match.fg" . ,blue)
                  ("colors.keyhint.bg" . ,background)
                  ("colors.keyhint.fg" . ,text)
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
                  ("colors.prompts.border" . "none")
                  ("colors.prompts.fg" . ,text)
                  ("colors.prompts.selected.bg" . ,magenta)
                  ("colors.statusbar.caret.bg" . ,cyan)
                  ("colors.statusbar.caret.fg" . ,cursor)
                  ("colors.statusbar.caret.selection.bg" . ,cyan)
                  ("colors.statusbar.caret.selection.fg" . ,text)
                  ("colors.statusbar.command.bg" . ,background)
                  ("colors.statusbar.command.fg" . ,text)
                  ("colors.statusbar.command.private.bg" . ,background)
                  ("colors.statusbar.command.private.fg" . ,text)
                  ("colors.statusbar.insert.bg" . ,green)
                  ("colors.statusbar.insert.fg" . ,background)
                  ("colors.statusbar.normal.bg" . ,background)
                  ("colors.statusbar.normal.fg" . ,text)
                  ("colors.statusbar.passthrough.bg" . ,blue)
                  ("colors.statusbar.passthrough.fg" . ,blue-text)
                  ("colors.statusbar.private.bg" . ,background)
                  ("colors.statusbar.private.fg" . ,text)
                  ("colors.statusbar.progress.bg" . ,text)
                  ("colors.statusbar.url.error.fg" . ,red)
                  ("colors.statusbar.url.fg" . ,text)
                  ("colors.statusbar.url.hover.fg" . ,blue)
                  ("colors.statusbar.url.success.http.fg" . ,text)
                  ("colors.statusbar.url.success.https.fg" . ,green)
                  ("colors.statusbar.url.warn.fg" . ,red)
                  ("colors.tabs.bar.bg" . ,background-offset)
                  ("colors.tabs.even.bg" . ,background-offset)
                  ("colors.tabs.even.fg" . ,disabled-text)
                  ("colors.tabs.indicator.error" . ,red)
                  ("colors.tabs.indicator.system" . "none")
                  ("colors.tabs.odd.bg" . ,background-offset)
                  ("colors.tabs.odd.fg" . ,disabled-text)
                  ("colors.tabs.selected.even.bg" . ,background)
                  ("colors.tabs.selected.even.fg" . ,text)
                  ("colors.tabs.selected.odd.bg" . ,background)
                  ("colors.tabs.selected.odd.fg" . ,text)
                  ("colors.contextmenu.disabled.bg" . ,background-offset)
                  ("colors.contextmenu.disabled.fg" . ,disabled-text)
                  ("colors.contextmenu.menu.bg" . ,background)
                  ("colors.contextmenu.menu.fg" . ,text)
                  ("colors.contextmenu.selected.bg" . ,primary)
                  ("colors.contextmenu.selected.fg" . ,primary-overlay-text)
                  ("colors.webpage.bg" . ,background)
                  ("colors.webpage.preferred_color_scheme"
                   . ,(if light? "light" "dark")))))))))
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
