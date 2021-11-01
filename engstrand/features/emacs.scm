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
      ; #:package emacs-pgtk-native-comp
      #:package emacs-next-pgtk
      #:additional-elisp-packages (list emacs-evil emacs-evil-collection emacs-doom-themes emacs-doom-modeline emacs-geiser emacs-geiser-guile)
      #:extra-config '((evil-mode 1)
                       (load-theme 'doom-one)
                       (fringe-mode '(0 . 0))
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
    (feature-emacs-keycast)
    (feature-emacs-git)
    (feature-emacs-pdf-tools)
    (feature-emacs-org)
    (feature-emacs-org-roam
      #:org-roam-directory "~/roam/")))

;                  (require 'use-package))
;                (use-package evil
;                             :init
;                             (setq evil-want-integration t)
;                             (setq evil-want-keybinding nil)
;                             (setq evil-want-C-u-scroll t)
;                             (setq evil-want-C-i-jump nil)
;                             :hook (evil-mode . rune/evil-hook)
;                             :config
;                             (evil-mode 1)
;                             (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
;                             (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join))
;                ; (use-package doom-modeline
;                ;              :ensure t
;                ;              :init (doom-modeline-mode 1)
;                ;              :custom ((doom-modeline-height 15)))
;                ))))
