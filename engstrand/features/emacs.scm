(define-module (engstrand features emacs)
  #:use-module (guix gexp)
  #:use-module (engstrand utils)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (flat packages emacs)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (rde features)
  #:use-module (rde features base)
  #:use-module (rde features emacs)
  #:export (
            feature-emacs-default-editor
            feature-emacs-org-latex-preview
            feature-emacs-corfu
            feature-emacs-dashboard
            feature-emacs-modus-themes
            feature-emacs-evil

            %engstrand-emacs-package
            %engstrand-emacs-base-features))

(define* (make-emacs-feature base-name
                        #:key
                        (home-services (const '()))
                        (system-services (const '())))
  "Creates a basic emacs feature configuration."
  (let ((f-name (symbol-append 'emacs- base-name)))
    (feature
     (name f-name)
     (values `((,f-name . #t)))
     (home-services-getter home-services)
     (system-services-getter system-services))))

(define* (feature-emacs-default-editor)
  "Configure emacs as the default system editor."

  (define (get-home-services config)
    (list
     (simple-service
      'set-emacs-environment-variables
      home-environment-variables-service-type
      `(("EDITOR" . ,(file-append %engstrand-emacs-package "/bin/emacs"))
        ;; Used by guix commands, e.g. guix edit. rde sets this by itself,
        ;; but the --no-wait option does not seem to play nice with our setup.
        ("VISUAL" . ,(get-value 'emacs-client-create-frame config))))))

  (feature
   (name 'emacs-default-editor)
   (home-services-getter get-home-services)))

(define* (feature-emacs-org-latex-preview)
  "Add and configure latex previews in Emacs Org mode."
  (define emacs-f-name 'org-latex-preview)

  (define (get-home-services config)
    (list
     (simple-service
      'add-org-mode-latex-preview-home-packages-to-profile
      home-profile-service-type
      (pkgs '("texlive" "texlive-latex-preview" "texlive-graphics-def")))
     (elisp-configuration-service
      emacs-f-name
      `((require 'org)
        ;; Use dvisvgm for latex rendering
        (setq org-latex-create-formula-image-program 'dvisvgm)
        ;; Increase latex preview scale in org mode
        (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.8))))))

  (make-emacs-feature emacs-f-name
                      #:home-services get-home-services))

(define* (feature-emacs-corfu
          ;; #:key
          ;; ()
          )
  "Add and configure Corfu completion for Emacs."
  (define emacs-f-name 'corfu)

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

  (make-emacs-feature emacs-f-name
                      #:home-services get-home-services))

(define* (feature-emacs-dashboard
          ;; #:key
          ;; (emacs-dashboard emacs-dashboard)
          )
  "Add and configure emacs-dashboard as a welcome screen."
  (define emacs-f-name 'dashboard)

  (define (get-home-services config)
    (list
     (elisp-configuration-service
      emacs-f-name
      `((eval-when-compile (require 'dashboard))
        (dashboard-setup-startup-hook)
        (setq dashboard-center-content t)
        (setq dashboard-set-init-info nil)
        (setq dashboard-set-footer nil)
        (setq dashboard-page-separator "\n\n")

        (eval-when-compile (require 'project))
        (setq dashboard-projects-backend 'project)
        )
      #:elisp-packages (list
                        emacs-dashboard
                        emacs-project
                        ;; Optional dependencies:
                        ;; emacs-projectile
                        ;; emacs-page-break-lines
                        ;; emacs-all-the-icons
                        ))))

  (make-emacs-feature emacs-f-name
                      #:home-services get-home-services))

(define* (feature-emacs-modus-themes
          ;; #:key
          ;; ()
          )
  "Add and configure the Modus themes for Emacs."
  (define emacs-f-name 'modus-themes)

  (define (get-home-services config)
    (list
     (elisp-configuration-service
      emacs-f-name
      `((require 'modus-themes)
        (window-divider-mode 0)
        (setq modus-themes-italic-constructs t
              modus-themes-bold-constructs t
              modus-themes-mixed-fonts t
              modus-themes-subtle-line-numbers t
              modus-themes-intense-markup t
              modus-themes-lang-checkers nil
              modus-themes-mode-line '(borderless)
              modus-themes-syntax nil
              modus-themes-hl-line '(underline intense)
              modus-themes-paren-match nil
              modus-themes-links nil
              modus-themes-prompts nil
              modus-themes-mail-citations 'faint
              modus-themes-region '(bg-only accented)
              modus-themes-diffs 'nil
              modus-themes-vivendi-color-overrides '((bg-main . "#0A0A0A"))
              modus-themes-org-blocks 'gray-background
              modus-themes-org-agenda '((header-block . (variable-pitch 1.3))
                                        (header-date . (grayscale workaholic bold-today 1.1))
                                        (event . (accented varied))
                                        (scheduled . uniform)
                                        (habit . traffic-light))
              modus-themes-headings '((1 . (background variable-pitch 1.3))
                                      (2 . (rainbow overline 1.1))
                                      (t . (semibold))))
        (modus-themes-load-themes)
        (modus-themes-load-vivendi))
      #:elisp-packages (list
                        emacs-modus-themes))))

  (make-emacs-feature emacs-f-name
                      #:home-services get-home-services))

(define* (feature-emacs-evil
          #:key
          (no-insert-state-message? #t)
          (leader? #t)
          (undo-fu? #t)
          (commentary? #t)
          (collection? #t)
          (surround? #t))
  "Add and configure evil-mode for Emacs."
  (ensure-pred boolean? no-insert-state-message?)
  (ensure-pred boolean? leader?)
  (ensure-pred boolean? undo-fu?)
  (ensure-pred boolean? collection?)
  (ensure-pred boolean? surround?)
  (define emacs-f-name 'evil)

  (define (get-home-services config)
    (list
     (elisp-configuration-service
      emacs-f-name
      `(;; Make the Escape key behave more nicely for evil-mode
        (global-set-key (kbd "<escape>") 'keyboard-quit)
        (define-key query-replace-map (kbd "<escape>") 'quit)
        ;; Hide ``-- INSERT --'' message
        ,@(if no-insert-state-message?
              `((setq evil-insert-state-message nil))
              '())
        ;; Required by the additional packages... should we toggle this?
        (setq evil-want-keybinding nil)
        ;; Use C-u to scroll up
        (setq evil-want-C-u-scroll t)
        ;; undo with higher granularity
        (setq evil-want-fine-undo t)
        ;; The packages below must be loaded and configured in a certain order
        (require 'evil)
        ,@(if leader?
              `((require 'evil-leader)
                (global-evil-leader-mode)
                (evil-leader/set-leader "<SPC>")
                (evil-leader/set-key
                 "<SPC>" 'find-file
                 "b" 'switch-to-buffer
                 "k" 'kill-buffer
                 "K" 'kill-this-buffer
                 "s" 'save-buffer
                 "S" 'evil-write-all
                 )
                '()))
        ,@(if undo-fu?
              `((eval-when-compile (require 'undo-fu))
                (setq evil-undo-system 'undo-fu)
                (define-key evil-normal-state-map (kbd "u") 'undo-fu-only-undo)
                (define-key evil-normal-state-map (kbd "C-r") 'undo-fu-only-redo))
              '())
        (evil-mode 1)
        ,@(if commentary?
              `((require 'evil-commentary)
                (evil-commentary-mode))
              '())
        ,@(if collection?
              `((when (require 'evil-collection nil t)
                  (evil-collection-init)))
              '())
        ,@(if surround?
              `((require 'evil-surround)
                (global-evil-surround-mode 1))
              '())
        )
      #:elisp-packages (list
                        emacs-evil
                        (if leader? emacs-evil-leader)
                        (if undo-fu? emacs-undo-fu)
                        (if commentary? emacs-evil-commentary)
                        (if collection? emacs-evil-collection)
                        (if surround? emacs-evil-surround)))))

  (make-emacs-feature emacs-f-name
                      #:home-services get-home-services))

(define %engstrand-emacs-package emacs-pgtk-native-comp)

(define %engstrand-emacs-base-features
  (list
   (feature-emacs-default-editor)
   (feature-emacs
    ;; #:emacs %engstrand-emacs-package
    #:additional-elisp-packages (list emacs-geiser emacs-geiser-guile)
    #:extra-init-el '(;; no fringes
                      (fringe-mode 0)
                      ;; do not open the Emacs welcome screen when we pass an
                      ;; existing file as a command-line argument
                      (defun my-inhibit-startup-screen-file ()
                        (ignore
                         (setq inhibit-startup-screen
                               (file-exists-p
                                (expand-file-name argi command-line-default-directory)))))
                      (add-hook 'command-line-functions 'my-inhibit-startup-screen-file)
                      ;; ignore warnings from native-comp
                      (setq native-comp-async-report-warnings-errors nil)
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
                      ;; Move to future C feature
                      (setq c-default-style "linux")
                      ;; Delete whitespace from indentations immediately
                      (setq backward-delete-char-untabify-method 'hungry)
                      ;; Transparency - laggy when window large
                      ;; (add-to-list 'default-frame-alist '(alpha 93 . 93))
                      ;; Clean up white space
                      (add-hook 'before-save-hook 'whitespace-cleanup)
                      ;; Allow execution of src blocks without asking
                      (setq org-confirm-babel-evaluate nil)))
   ;; Load custom theme before rde emacs-appearance feature.
   ;; This make sures that our custom settings overrides
   ;; any values set in rde.
   (feature-emacs-modus-themes)
   (feature-emacs-appearance
    #:margin 5)
   (feature-emacs-dashboard)
   (feature-emacs-evil)
   (feature-emacs-monocle
    #:olivetti-body-width 100)
   (feature-emacs-dired)
   (feature-emacs-faces)
   (feature-emacs-completion
    #:mini-frame? #f)
   (feature-emacs-corfu)
   (feature-emacs-vertico)
   (feature-emacs-project)
   (feature-emacs-perspective)
   (feature-emacs-which-key)
   (feature-emacs-git)
   (feature-emacs-keycast)
   (feature-emacs-pdf-tools)
   (feature-emacs-org)
   (feature-emacs-org-agenda)
   (feature-emacs-org-latex-preview)
   (feature-emacs-org-roam
    #:org-roam-directory "~/roam/")))
