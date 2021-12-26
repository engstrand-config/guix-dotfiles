(define-module (engstrand features emacs)
  #:use-module (engstrand utils)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (flat packages emacs)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:export (feature-emacs-evil
            %engstrand-emacs-base-features))

;; (define* (feature-emacs-company
;;           ;; #:key
;;           ;; ()
;;           )
;;   "Add and configure Company completion for Emacs."
;;   (define emacs-f-name 'company)
;;   (define f-name (symbol-append 'emacs- emacs-f-name))

;;   (define (get-home-services config)
;;     (list
;;      (elisp-configuration-service
;;       emacs-f-name
;;       `((require 'company)
;;         (require 'company-dabbrev-code)
;;         (add-hook 'after-init-hook 'global-company-mode)
;;         (setq company-idle-delay 0)
;;         (setq company-minimum-prefix-length 1)
;;         (setq company-selection-wrap-around t)
;;         (setq company-backend 'company-dabbrev-code)
;;         (company-tng-configure-default))
;;       #:elisp-packages (list
;;                         emacs-company))))
;;   (feature
;;    (name f-name)
;;    (values `((,f-name . #t)))
;;    (home-services-getter get-home-services)))

(define* (feature-emacs-corfu
          ;; #:key
          ;; ()
          )
  "Add and configure Corfu completion for Emacs."
  (define emacs-f-name 'corfu)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (elisp-configuration-service
      emacs-f-name
      `((require 'corfu)
        ;; TAB-and-Go completion
        (setq corfu-cycle t)
        (setq corfu-preselect-first nil)
        (setq corfu-auto t)
        (corfu-global-mode 1)
        (define-key corfu-map (kbd "<tab>") 'corfu-next)
        (define-key corfu-map (kbd "<backtab>") 'corfu-previous))
      #:elisp-packages (list
                        emacs-corfu))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-evil
          #:key
          (evil-collection? #t)
          (evil-surround? #t))
  "Add and configure evil-mode for Emacs."
  (ensure-pred boolean? evil-collection?)
  (ensure-pred boolean? evil-surround?)
  (define emacs-f-name 'evil)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (elisp-configuration-service
      emacs-f-name
      `((setq evil-want-keybinding nil)
        (setq evil-want-C-u-scroll t)
        (setq evil-toggle-key "C-`")
        (require 'evil)
        (evil-mode 1)
        (when (require 'evil-collection nil t)
          (evil-collection-init))
        (require 'evil-commentary)
        (evil-commentary-mode)
        (require 'evil-surround)
        (global-evil-surround-mode 1)
        (define-key key-translation-map (kbd "ESC") (kbd "C-g"))
        (setq evil-insert-state-message nil))
      #:elisp-packages (list
                        emacs-evil
                        emacs-evil-commentary
                        (if evil-collection? emacs-evil-collection)
                        (if evil-surround? emacs-evil-surround)))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define %engstrand-emacs-base-features
  (list
   (feature-emacs
    #:emacs emacs-next-pgtk
    #:additional-elisp-packages (list emacs-geiser emacs-geiser-guile)
    #:extra-init-el '((fringe-mode 0)
                      ;; Undo the top modeline of emacs-appearance
                      (setq-default mode-line-format header-line-format)
                      (setq-default header-line-format nil)
                      ;; Relative line numbers, but only when relevant
                      (setq-default display-line-numbers-type 'relative)
                      (add-hook 'prog-mode-hook 'display-line-numbers-mode)
                      ;; Olivetti mode when working with text
                      (add-hook 'text-mode-hook 'olivetti-mode)
                      ;; Nicer mouse scrolling
                      (setq mouse-wheel-progressive-speed nil)
                      (setq mouse-wheel-scroll-amount '(3))
                      ;; Allow execution of src blocks without asking
                      (setq org-confirm-babel-evaluate nil)))
   (feature-emacs-appearance
    #:margin 5)
   (feature-emacs-evil)
   (feature-emacs-monocle)
   (feature-emacs-dired)
   (feature-emacs-faces)
   (feature-emacs-completion
    #:mini-frame? #f)
   ;; (feature-emacs-auto-complete)
   ;; (feature-emacs-company)
   (feature-emacs-corfu)
   (feature-emacs-vertico)
   (feature-emacs-project)
   (feature-emacs-perspective)
   (feature-emacs-input-methods)
   (feature-emacs-which-key)
   (feature-emacs-git)
   (feature-emacs-keycast)
   (feature-emacs-pdf-tools)
   (feature-emacs-org)
   (feature-emacs-org-agenda)
   (feature-emacs-org-roam
    #:org-roam-directory "~/roam/")))
