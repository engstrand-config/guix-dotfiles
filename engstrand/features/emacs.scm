(define-module (engstrand features emacs)
  #:use-module (engstrand utils)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (flat packages emacs)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:export (feature-emacs-corfu
            feature-emacs-evil
            %engstrand-emacs-base-features))

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
              modus-themes-org-agenda '((header-block . (variable-pitch 1.3))
                                        (header-date . (grayscale workaholic bold-today 1.1))
                                        (event . (accented varied))
                                        (scheduled . uniform)
                                        (habit . traffic-light))
              modus-themes-headings '((1 . (background variable-pitch 1.3))
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
          (insert-state-message? #f)
          (leader? #t)
          (commentary? #t)
          (collection? #t)
          (surround? #t))
  "Add and configure evil-mode for Emacs."
  (ensure-pred boolean? insert-state-message?)
  (ensure-pred boolean? leader?)
  (ensure-pred boolean? collection?)
  (ensure-pred boolean? surround?)
  (define emacs-f-name 'evil)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (elisp-configuration-service
      emacs-f-name
      `(;; Make the Escape key behave more nicely for evil-mode
        (global-set-key (kbd "<escape>") 'keyboard-quit)
        (define-key query-replace-map (kbd "<escape>") 'quit)
        ;; Required by the additional packages... should we toggle this?
        (setq evil-want-keybinding nil)
        ;; Hide ``-- INSERT --'' message
        ,@(if insert-state-message?
              `((setq evil-insert-state-message nil))
              '())
        ;; Use C-u to scroll up
        (setq evil-want-C-u-scroll t)
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
                        (if commentary? emacs-evil-commentary)
                        (if collection? emacs-evil-collection)
                        (if surround? emacs-evil-surround)))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define %engstrand-emacs-base-features
  (list
   (feature-emacs
    #:emacs emacs-pgtk-native-comp
    #:additional-elisp-packages (list emacs-geiser emacs-geiser-guile)
    #:extra-init-el '((fringe-mode 0)
                      (setq native-comp-async-report-warnings-errors nil)
                      ;; Undo the top modeline of emacs-appearance
                      (setq-default mode-line-format header-line-format)
                      (setq-default header-line-format nil)
                      ;; Relative line numbers, but only when relevant
                      (setq-default display-line-numbers-type 'relative)
                      (add-hook 'prog-mode-hook 'display-line-numbers-mode)
                      ;; Olivetti mode when working with text
                      (add-hook 'text-mode-hook 'olivetti-mode)
                      (setq olivetti-set-width 100)
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
   (feature-emacs-appearance
    #:margin 5)
   (feature-emacs-evil)
   (feature-emacs-monocle)
   (feature-emacs-dired)
   (feature-emacs-faces)
   (feature-emacs-completion
    #:mini-frame? #f)
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
