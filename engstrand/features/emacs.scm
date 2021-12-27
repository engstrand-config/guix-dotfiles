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

(define* (feature-emacs-modus-themes
          ;; #:key
          ;; ()
          )
  "Add and configure the Modus themes for Emacs."
  (define emacs-f-name 'modus-themes)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (elisp-configuration-service
      emacs-f-name
      `((require 'modus-themes)
        (setq modus-themes-italic-constructs t
              modus-themes-bold-constructs t
              modus-themes-mixed-fonts t
              modus-themes-subtle-line-numbers t
              modus-themes-intense-markup t

              ;; Options for `modus-themes-lang-checkers' are either nil (the
              ;; default), or a list of properties that may include any of those
              ;; symbols: `straight-underline', `text-also', `background',
              ;; `intense' OR `faint'.
              modus-themes-lang-checkers nil

              ;; Options for `modus-themes-mode-line' are either nil, or a list
              ;; that can combine any of `3d' OR `moody', `borderless',
              ;; `accented', and a natural number for extra padding
              modus-themes-mode-line '(borderless)

              ;; Options for `modus-themes-syntax' are either nil (the default),
              ;; or a list of properties that may include any of those symbols:
              ;; `faint', `yellow-comments', `green-strings', `alt-syntax'
              modus-themes-syntax nil

              ;; Options for `modus-themes-hl-line' are either nil (the default),
              ;; or a list of properties that may include any of those symbols:
              ;; `accented', `underline', `intense'
              modus-themes-hl-line '(underline intense)

              ;; Options for `modus-themes-paren-match' are either nil (the
              ;; default), or a list of properties that may include any of those
              ;; symbols: `bold', `intense', `underline'
              modus-themes-paren-match nil

              ;; Options for `modus-themes-links' are either nil (the default),
              ;; or a list of properties that may include any of those symbols:
              ;; `neutral-underline' OR `no-underline', `faint' OR `no-color',
              ;; `bold', `italic', `background'
              modus-themes-links nil

              ;; Options for `modus-themes-prompts' are either nil (the
              ;; default), or a list of properties that may include any of those
              ;; symbols: `background', `bold', `gray', `intense', `italic'
              modus-themes-prompts nil

              modus-themes-mail-citations 'faint ; {nil,'faint,'monochrome}

              ;; Options for `modus-themes-region' are either nil (the default),
              ;; or a list of properties that may include any of those symbols:
              ;; `no-extend', `bg-only', `accented'
              modus-themes-region '(bg-only accented)

              ;; Options for `modus-themes-diffs': nil, 'desaturated, 'bg-only
              modus-themes-diffs 'nil

              modus-themes-org-blocks 'gray-background ; {nil,'gray-background,'tinted-background}

              modus-themes-org-agenda ; this is an alist: read the manual or its doc string
              '((header-block . (variable-pitch 1.3))
                (header-date . (grayscale workaholic bold-today 1.1))
                (event . (accented varied))
                (scheduled . uniform)
                (habit . traffic-light))

              modus-themes-headings ; this is an alist: read the manual or its doc string
              '((1 . (background variable-pitch 1.3))
                (2 . (rainbow overline 1.1))
                (t . (semibold))))
        (modus-themes-load-themes))
      #:elisp-packages (list
                        emacs-modus-themes))))

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
        ;; Make the Escape key behave more nicely for evil-mode
	(global-set-key (kbd "<escape>") 'keyboard-quit)
        (define-key query-replace-map (kbd "<escape>") 'quit)

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
                      ;; Configure the look of  tabs
                      (setq tab-bar-close-button-show nil
                            tab-bar-new-button-show nil
                            tab-bar-new-tab-choice "*scratch*")
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
   (feature-emacs-modus-themes)
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
