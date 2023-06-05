(define-module (engstrand features browsers)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (nongnu packages mozilla)
  #:use-module (dwl-guile utils)
  #:use-module (dwl-guile home-service)
  #:use-module (gnu packages web-browsers)
  #:use-module (farg colors)
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
   ((list? value) (fold
                   (lambda (x acc)
                     (if (eq? acc "")
                         (str-escape x)
                         (string-append acc ", " (str-escape x))))
                   ""
                   value))
   (else value)))

(define* (serialize-qutebrowser-config alist)
  "Serializes an alist of qutebrowser options into a config.py"
  (fold
   (lambda (entry acc)
     (let ((str-value (serialize-qutebrowser-value (cdr entry))))
       (if (eq? (car entry) "bind")
           (string-append acc "config.bind(" str-value ")\n")
           (string-append acc "c." (car entry) " = " str-value "\n"))))
   "config.load_autoconfig()\n"
   alist))

(define* (feature-firefox
          #:key
          (package firefox/wayland)
          (open-key "S-s-w")
          (default-browser? #f))
  "Setup Firefox."

  (ensure-pred string? open-key)
  (ensure-pred package? package)
  (ensure-pred boolean? default-browser?)

  (define (get-home-services config)
    "Return a list of home services required by Firefox."
    (let ((package (if (get-value 'wayland config) firefox/wayland firefox)))
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
          `((set-keys ,open-key
                      (lambda ()
                        (dwl:spawn ,(file-append package "/bin/firefox"))))))))))

  (feature
   (name 'firefox)
   (home-services-getter get-home-services)))

(define* (feature-qutebrowser
          #:key
          (package qutebrowser/wayland)
          (open-key "S-s-w")
          (auto-fill-bindings? #t)
          (default-browser? #f))
  "Setup qutebrowser, a keyboard-focused browser with a minimal GUI."

  (ensure-pred package? package)
  (ensure-pred string? open-key)
  (ensure-pred boolean? auto-fill-bindings?)
  (ensure-pred boolean? default-browser?)

  (lambda (_ palette)
    (define (get-home-services config)
      "Return a list of home services required by qutebrowser"
      (make-service-list
       (when default-browser?
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
             (let* ((light? (palette 'light?))
                    (text (palette 'fg))
                    (cursor text)
                    (background (palette 'bg))
                    (background-offset (palette 'bg-alt))
                    (primary (palette 'accent-0))
                    (primary-text (palette 'accent-0-text))
                    (primary-overlay-text
                     (farg:make-readable primary primary))
                    (disabled-text
                     (farg:make-readable background-offset background-offset))
                    (red (palette 'red))
                    (green (palette 'green))
                    (blue (palette 'blue))
                    (yellow (palette 'yellow))
                    (magenta (palette 'magenta))
                    (cyan (palette 'cyan))
                    (red-text (palette 'red-text))
                    (green-text (palette 'green-text))
                    (blue-text (palette 'blue-text))
                    (yellow-text (palette 'yellow-text))
                    (magenta-text (palette 'magenta-text))
                    (cyan-text (palette 'cyan-text)))
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
                  ("colors.completion.item.selected.match.fg" . ,primary-text)
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
                  ("colors.statusbar.caret.fg" . ,cyan-text)
                  ("colors.statusbar.caret.selection.bg" . ,cyan)
                  ("colors.statusbar.caret.selection.fg" . ,cyan-text)
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
                  ;; Websites that does not explicitly set the background color
                  ;; will use this color instead. This is problematic since
                  ;; they expect the default background to be white, thus resulting
                  ;; in unreadable text.
                  ("colors.webpage.bg" . "#ffffff")
                  ("colors.webpage.preferred_color_scheme"
                   . ,(if light? "light" "dark"))
                  ,@(if auto-fill-bindings?
                        '(("bind" .
                           ("<Ctrl-Shift-i>"
                            "spawn --userscript qute-rbw"
                            "insert"))
                          ("bind" .
                           ("<Ctrl-Shift-p>"
                            "spawn --userscript qute-rbw --password-only"
                            "insert"))
                          ("bind" .
                           ("<Ctrl-Shift-u>"
                            "spawn --userscript qute-rbw --username-only"
                            "insert")))
                        '()))))))))
       (simple-service
        'reload-qutebrowser-on-farg-activation
        home-farg-service-type
        (list
         #~(begin
             (use-modules (ice-9 popen))
             (display "Reloading qutebrowser to update theme...\n")
             (let* ((dir (getenv "XDG_RUNTIME_DIR"))
                    (qutefiles (scandir (string-append dir "/qutebrowser")))
                    (files (if qutefiles (length qutefiles) 0)))
               (when (> files 2) ; FIFO file is available, instance is running
                 (system* #$(file-append package "/bin/qutebrowser")
                          ":config-source"
                          ;; Only output potential errors
                          "-l=critical"))))))
       (when (and default-browser? (get-value 'dwl-guile config))
         (simple-service
          'add-qutebrowser-dwl-keybindings
          home-dwl-guile-service-type
          `((set-keys ,open-key
                      (lambda ()
                        (dwl:spawn ,(file-append package "/bin/qutebrowser")
                                   "--qt-arg" "no-sandbox" "true"))))))))

    (feature
     (name 'qutebrowser)
     (home-services-getter get-home-services))))
