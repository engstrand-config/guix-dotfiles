(define-module (engstrand features emacs)
               #:use-module (engstrand utils)
               #:use-module (gnu home services)
               #:use-module (gnu services)
               #:use-module (flat packages emacs)
               #:use-module (gnu packages emacs)
               #:use-module (gnu packages emacs-xyz)
               #:use-module (rde features)
               #:use-module (rde features emacs)
               #:export (%engstrand-emacs-base-features feature-emacs-guix))


(define* (feature-emacs-guix)
         "Add the emacs-guix package."

         (define (get-home-services config)
           (list
             (simple-service
               'add-emacs-home-packages-to-profile
               home-profile-service-type
               (pkgs '("emacs-guix")))))

         (feature
           (name 'emacs-guix)
           (home-services-getter get-home-services)))

(define %engstrand-emacs-base-features
  (list
   (feature-emacs
    #:emacs emacs-next-pgtk
    #:additional-elisp-packages (list emacs-evil emacs-evil-collection emacs-geiser emacs-geiser-guile)
    #:extra-init-el '((evil-mode 1)
                      (fringe-mode '(0 . 0))
                      (setq-default display-line-numbers-type 'relative)
                      (global-display-line-numbers-mode)
                      (define-key key-translation-map (kbd "ESC") (kbd "C-g"))
                      ; (evil-collection-init)
                      ))
   (feature-emacs-guix)
   (feature-emacs-appearance
    #:margin 8)
   (feature-emacs-monocle)
   (feature-emacs-dired)
   (feature-emacs-faces)
   (feature-emacs-completion)
   (feature-emacs-project)
   (feature-emacs-perspective)
   (feature-emacs-input-methods)
   (feature-emacs-which-key)
   (feature-emacs-git)
   (feature-emacs-keycast)
   (feature-emacs-pdf-tools)
   (feature-emacs-org)
   (feature-emacs-org-roam
    #:org-roam-directory "~/roam/"))) ; TODO: add roam dir in guix home
