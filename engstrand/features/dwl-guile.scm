(define-module (engstrand features dwl-guile)
  #:use-module (rde features)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (farg colors)
  #:use-module (dwl-guile utils)
  #:use-module (dwl-guile patches)
  #:use-module (dwl-guile home-service)
  #:export (
            feature-dwl-guile
            has-dwl-patch?
            %engstrand-dwl-guile-patches))

;; Checks if SYMBOL corresponds to a patch that is/will
;; be applied to dwl-guile, based on the feature values in CONFIG.
;; SYMBOL should be the name of the patch, not including the ".patch" extension.
;; I.e. @code{(has-dwl-patch? 'xwayland config)}.
(define (has-dwl-patch? symbol config)
  (let ((patch-name (string-append (symbol->string symbol) ".patch")))
    (find (lambda (p) (equal? patch-name (local-file-name p)))
          (get-value 'dwl-guile-patches config))))

(define %engstrand-dwl-guile-patches
  (list %patch-xwayland
        %patch-swallow
        %patch-movestack
        %patch-attachabove
        %patch-focusmonpointer
        %patch-monitor-config))

(define* (feature-dwl-guile
          #:key
          (repl? #t)
          (repl-key "s-S-C-i"))
  "Setup dwl-guile."

  (ensure-pred boolean? repl?)
  (ensure-pred string? repl-key)

  (define (get-emacs-start-repl-exp)
    (object->string
      '(progn
        (require 'geiser-repl)
        (geiser-repl--start-repl
         (geiser-repl--get-impl "Connect to Scheme impl: ")
         ,%dwl:repl-socket-path)
        (kill-buffer "*scratch*")
        (delete-other-windows))))

  (lambda (_ palette)
    (define (get-home-services config)
      "Return a list of home services required by dwl-guile."
      (list
       (service home-dwl-guile-service-type
                (home-dwl-guile-configuration
                 (package
                  (patch-dwl-guile-package dwl-guile
                                           #:patches %engstrand-dwl-guile-patches))
                 (config
                  (list
                   `((setq root-color ,(palette 'bg)
                           border-color ,(farg:offset (palette 'bg) 10)
                           focus-color ,(palette 'accent-0)
                           lockscreen-color ,(farg:with-alpha (palette 'bg) 90)
                           border-px 2
                           sloppy-focus? #t
                           smart-gaps? #t
                           smart-borders? #t)

                     ,@(if repl?
                           `((set-keys ,repl-key
                                       `(dwl:spawn "emacs" "--eval"
                                                   ,(get-emacs-start-repl-exp)))
                             (add-hook! dwl:hook-startup dwl:start-repl-server))
                           '()))))))))

    (feature
     (name 'wayland-dwl-guile)
     (values `((wayland . #t)
               (dwl-guile . #t)
               (dwl-guile-patches . ,%engstrand-dwl-guile-patches)))
     (home-services-getter get-home-services))))
