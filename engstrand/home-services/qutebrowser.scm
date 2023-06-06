(define-module (engstrand home-services qutebrowser)
  #:use-module (gnu home services)
  #:use-module (gnu home services xdg)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages web-browsers)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix diagnostics)
  #:use-module (rde features predicates)
  #:use-module (engstrand serializers qutebrowser)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (home-qutebrowser-service-type
            home-qutebrowser-configuration
            home-qutebrowser-configuration?
            home-qutebrowser-configuration-package
            home-qutebrowser-configuration-properties
            home-qutebrowser-configuration-bindings
            home-qutebrowser-configuration-userscripts
            home-qutebrowser-configuration-reload-config-on-change?
            home-qutebrowser-configuration-default-browser?

            home-qutebrowser-extension
            home-qutebrowser-extension?
            home-qutebrowser-extension-properties
            home-qutebrowser-extension-bindings
            home-qutebrowser-extension-userscripts))

(define-configuration home-qutebrowser-configuration
  (package
   (package qutebrowser)
   "Which qutebrowser package to use.")
  (properties
   (alist '())
   "Properties to set for qutebrowser. The contents will be
serialized into a python file. Key bindings should be placed in the
@code{bindings} field, not here.")
  (bindings
   (alist '())
   "Additional keybindings to add to qutebrowser.")
  (userscripts
   (alist '())
   "List of additional userscripts to install. Each userscript will
be installed into $XDG_CONFIG_HOME/qutebrowser/userscripts/<name>, and
the appropriate execution permissions will be set.

The alist should contain entries with two values, the first being the
name of the userscript, and the second a file-like gexp.")
  (reload-config-on-change?
   (boolean #t)
   "If the config should be automatically reloaded in any running
qutebrowser instances after running a home reconfigure.")
  (default-browser?
    (boolean #f)
    "If qutebrowser should be used as the default web browser.")
  (no-serialization))

(define-configuration home-qutebrowser-extension
  (properties
   (alist '())
   "Properties to set for qutebrowser. The contents will be
serialized into a python file. Key bindings should be placed in the
@code{bindings} field, not here.")
  (bindings
   (alist '())
   "Additional keybindings to add to qutebrowser.")
  (userscripts
   (alist '())
   "List of additional userscripts to install. Each userscript will
be installed into $XDG_CONFIG_HOME/qutebrowser/userscripts/<name>, and
the appropriate execution permissions will be set.

The alist should contain entries with two values, the first being the
name of the userscript, and the second a file-like gexp.")
  (no-serialization))

(define (home-qutebrowser-profile-service config)
  (list (home-qutebrowser-configuration-package config)))

(define (home-qutebrowser-environment-variables-service config)
  (if (home-qutebrowser-configuration-default-browser? config)
      `(("BROWSER" . ,(file-append (home-qutebrowser-configuration-package config)
                                   "/bin/qutebrowser")))
      '()))

(define (home-qutebrowser-xdg-mime-applications-service config)
  (home-xdg-mime-applications-configuration
   (default
     '((x-scheme-handler/http . org.qutebrowser.qutebrowser.desktop)
       (x-scheme-handler/https . org.qutebrowser.qutebrowser.desktop)
       (x-scheme-handler/about . org.qutebrowser.qutebrowser.desktop)
       (text/html . org.qutebrowser.qutebrowser.desktop)))))

(define (home-qutebrowser-xdg-configuration-files-service config)
  `(("qutebrowser/config.py"
     ,(mixed-text-file
       "qutebrowser-config.py"
       (serialize-qutebrowser-config (home-qutebrowser-configuration-properties config)
                                     (home-qutebrowser-configuration-bindings config))))
    ,@(map (lambda (script)
             `(,(string-append "qutebrowser/userscripts/" (car script))
               ,(computed-file
                 (car script)
                 (with-imported-modules
                  '((guix build utils))
                  #~(begin
                      (use-modules (guix build utils))
                      (copy-file #$(cdr script) #$output)
                      (chmod #$output #o555))))))
           (home-qutebrowser-configuration-userscripts config))))

(define (home-qutebrowser-on-change-service config)
  (let ((pkg (home-qutebrowser-configuration-package config)))
    (if (home-qutebrowser-configuration-reload-config-on-change? config)
        `(("files/.config/qutebrowser/config.py"
           ;; TODO: This one sometimes reloads qutebrowser even though it shouldnt
           ,#~(begin
                (use-modules (ice-9 popen))
                (display "Reloading qutebrowser config...\n")
                (let* ((dir (getenv "XDG_RUNTIME_DIR"))
                       (qutefiles (scandir (string-append dir "/qutebrowser")))
                       (files (if qutefiles (length qutefiles) 0)))
                  (when (> files 2) ; FIFO file is available, instance is running
                    (system* #$(file-append pkg "/bin/qutebrowser")
                             ":config-source"
                             ;; Only output potential errors
                             "-l=critical"))))))
        '())))

(define (home-qutebrowser-extensions original-config extensions)
  (let ((extensions (reverse extensions)))
    (home-qutebrowser-configuration
     (inherit original-config)
     (properties
      (append (home-qutebrowser-configuration-properties original-config)
              (append-map home-qutebrowser-extension-properties extensions)))
     (bindings
      (append (home-qutebrowser-configuration-bindings original-config)
              (append-map home-qutebrowser-extension-bindings extensions)))
     (userscripts
      (append (home-qutebrowser-configuration-userscripts original-config)
              (append-map home-qutebrowser-extension-userscripts extensions))))))

(define home-qutebrowser-service-type
  (service-type
   (name 'home-qutebrowser)
   (extensions
    (list
     (service-extension
      home-profile-service-type
      home-qutebrowser-profile-service)
     (service-extension
      home-environment-variables-service-type
      home-qutebrowser-environment-variables-service)
     (service-extension
      home-xdg-mime-applications-service-type
      home-qutebrowser-xdg-mime-applications-service)
     (service-extension
      home-xdg-configuration-files-service-type
      home-qutebrowser-xdg-configuration-files-service)
     (service-extension
      home-run-on-change-service-type
      home-qutebrowser-on-change-service)))
   (compose identity)
   (extend home-qutebrowser-extensions)
   (default-value (home-qutebrowser-configuration))
   (description "Configure and install qutebrowser")))
