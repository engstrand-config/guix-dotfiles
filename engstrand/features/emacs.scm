(define-module (engstrand features emacs)
  #:use-module (guix gexp)
  #:use-module (dwl-guile home-service)
  #:use-module (farg colors)
  #:use-module (farg home-service)
  #:use-module (engstrand utils)
  #:use-module (gnu home services)
  #:use-module (rde home services emacs)
  #:use-module (gnu services)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (rde gexp)
  #:use-module (rde features)
  #:use-module (rde features base)
  #:use-module (rde features emacs)
  #:use-module (rde features emacs-xyz)
  #:use-module (contrib features emacs-xyz)
  #:use-module (rde packages emacs)
  #:export (
            feature-emacs-default-editor
            feature-emacs-org-latex-preview
            feature-emacs-corfu
            feature-emacs-corfu-dabbrev
            feature-emacs-dashboard
            feature-emacs-transparency
            feature-emacs-engstrand-appearance

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

  (lambda (_ palette)
    (define (get-home-services config)
      (make-service-list
       (simple-service
        'set-emacs-environment-variables
        home-environment-variables-service-type
        `(("EDITOR" . ,(file-append %engstrand-emacs-package "/bin/emacs"))
          ;; Used by guix commands, e.g. guix edit. rde sets this by itself,
          ;; but the --no-wait option does not seem to play nice with our setup.
          ("VISUAL" . ,(get-value 'emacs-client-create-frame config))))))

    (feature
     (name 'emacs-default-editor)
     (home-services-getter get-home-services))))

(define* (feature-emacs-org-latex-preview)
  "Add and configure latex previews in Emacs Org mode."
  (define emacs-f-name 'org-latex-preview)

  (define (get-home-services config)
    (list
     (simple-service
      'add-org-mode-latex-preview-home-packages-to-profile
      home-profile-service-type
      (pkgs '("texlive" "texlive-latex-preview" "texlive-graphics-def")))
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((require 'org)
        ;; Use dvisvgm for latex rendering
        (setq org-latex-create-formula-image-program 'dvisvgm)
        ;; Increase latex preview scale in org mode
        (setq org-format-latex-options (plist-put org-format-latex-options :scale 1))))))

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
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((require 'corfu)
        ;; TAB-and-Go completion
        (setq corfu-cycle t)
        (setq corfu-preselect-first nil)
        (setq corfu-auto t)
        (global-corfu-mode 1)
        (define-key corfu-map (kbd "<tab>") 'corfu-next)
        (define-key corfu-map (kbd "<backtab>") 'corfu-previous))
      #:elisp-packages (list
                        emacs-corfu))))

  (make-emacs-feature emacs-f-name
                      #:home-services get-home-services))

(define* (feature-emacs-corfu-dabbrev
          #:key
          (completion-key "M-<tab>")
          (expand-key "M-C-<tab>"))
  "Switches the default dabbrev keybindings for usage with Corfu."
  (define emacs-f-name 'corfu-dabbrev)

  (ensure-pred string? completion-key)
  (ensure-pred string? expand-key)

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((require 'dabbrev)
        (global-set-key (kbd ,completion-key) 'dabbrev-completion)
        (global-set-key (kbd ,expand-key) 'dabbrev-expand)))))

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
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((require 'dashboard)
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

(define* (feature-emacs-transparency
          #:key
          (alpha #f))
  "Set frame background transparency."
  (define emacs-f-name 'transparency)
  (lambda (_ palette)
    (define (get-home-services config)
      (let ((emacs-alpha (if alpha alpha (palette 'alpha))))
        (list
         (rde-elisp-configuration-service
          emacs-f-name
          config
          `((add-to-list 'default-frame-alist '(alpha-background . ,emacs-alpha))
            ;; remove non-transparent border around frame
            (set-frame-parameter (selected-frame) 'internal-border-width 0))))))

    (make-emacs-feature emacs-f-name
                        #:home-services get-home-services)))

(define* (feature-emacs-engstrand-appearance)
  "Override default rde Emacs appearance."
  (define emacs-f-name 'engstrand-appearance)

  (lambda (_ palette)
    (define (get-home-services config)
      (list
       (rde-elisp-configuration-service
        emacs-f-name
        config
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
                modus-themes-org-blocks 'gray-background
                modus-themes-org-agenda
                '((header-block . (variable-pitch 1.3))
                  (header-date . (grayscale workaholic bold-today 1.1))
                  (event . (accented varied))
                  (scheduled . uniform)
                  (habit . traffic-light))
                modus-themes-headings
                '((1 . (background variable-pitch 1.3))
                  (2 . (rainbow overline 1.1))
                  (t . (semibold))))

          (setq modus-themes-common-palette-overrides
                `((fg-line-number-inactive "gray50")
                  (fg-line-number-active fg-main)
                  (bg-line-number-inactive bg-main)
                  (bg-line-number-active bg-main)
                  (border-mode-line-active unspecified)
                  (border-mode-line-inactive unspecified)
                  (fringe unspecified)))
          (load-theme 'modus-operandi t t)
          (load-theme 'modus-vivendi t t)
          (enable-theme ',(if (palette 'light?) 'modus-operandi 'modus-vivendi)))
          #:elisp-packages (list emacs-modus-themes))))

    (make-emacs-feature emacs-f-name
                        #:home-services get-home-services)))

(define %engstrand-emacs-package emacs-next-pgtk-latest)

(define %engstrand-emacs-base-features
  (list
   (feature-emacs-default-editor)
   (feature-emacs
    #:emacs %engstrand-emacs-package
    #:additional-elisp-packages (list emacs-geiser emacs-geiser-guile)
    #:extra-init-el '(;; do not open the Emacs welcome screen when we pass an
                      ;; existing file as a command-line argument
                      (defun my-inhibit-startup-screen-file ()
                        (ignore
                         (setq inhibit-startup-screen
                               (file-exists-p
                                (expand-file-name argi command-line-default-directory)))))
                      ;; always open the normal switch-to-buffe
                      (global-set-key (kbd "C-x C-b") 'switch-to-buffer)
                      (add-hook 'command-line-functions 'my-inhibit-startup-screen-file)
                      ;; ignore warnings from native-comp
                      ;; (setq native-comp-async-report-warnings-errors nil)
                      ;; Relative line numbers, but only when relevant
                      (setq-default display-line-numbers-type 'relative)
                      (add-hook 'prog-mode-hook 'display-line-numbers-mode)
                      ;; Olivetti mode when working with text
                      ;; (add-hook 'text-mode-hook 'olivetti-mode)
                      (global-olivetti-mode 1)
                      ;; Nicer mouse scrolling
                      (setq mouse-wheel-progressive-speed nil)
                      (setq mouse-wheel-scroll-amount '(3))
                      ;; Configure the look of  tabs
                      (setq tab-bar-close-button-show nil
                            tab-bar-new-button-show nil
                            tab-bar-new-tab-choice "*scratch*")
                      ;; Move to future C feature
                      (setq c-default-style "linux")
                      (add-hook 'c-mode-common-hook '(lambda () (setq indent-tabs-mode t)))
                      ;; Delete whitespace from indentations immediately
                      (setq backward-delete-char-untabify-method 'hungry)
                      ;; Clean up white space
                      (add-hook 'before-save-hook 'whitespace-cleanup)
                      ;; Allow execution of src blocks without asking
                      (setq org-confirm-babel-evaluate nil)
                      ;; (Temporarily) suppress startup warning in perspective.el
                      (setq persp-suppress-no-prefix-key-warning t)
                      ;; TODO: set in rde evil feature
                      (setq evil-want-minibuffer nil)
                      ;; for some reason this must be added manually
                      (vertico-mode)))
   (feature-emacs-appearance
    #:margin 5
    #:header-line-as-mode-line? #f)
   (feature-emacs-modus-themes
    #:deuteranopia? #f)
   (feature-emacs-engstrand-appearance)
   (feature-emacs-transparency)
   (feature-emacs-dashboard)
   (feature-emacs-evil
    #:hide-state-message? #t)
   (feature-emacs-monocle
    #:olivetti-body-width 100)
   (feature-emacs-dired)
   (feature-emacs-faces)
   (feature-emacs-completion
    #:mini-frame? #f)
   (feature-emacs-corfu)
   (feature-emacs-corfu-dabbrev)
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
