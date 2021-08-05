(define-module (engstrand features emacs))

; TODO: Integrate with rde emacs features
;       (service home-emacs-service-type
;                (home-emacs-configuration
;                  (package emacs-pgtk-native-comp)
;                  ; (rebuild-elisp-packages? #t)
;                  (xdg-flavor? #t)
;                  (server-mode? #t)
;                  (elisp-packages
;                    (list
;                      emacs-use-package
;                      emacs-evil
;                      emacs-guix
;                      emacs-doom-modeline
;                      emacs-geiser
;                      emacs-geiser-guile
;                      ))
;                  (init-el
;                    '(
;                      (package-initialize)
;                      (eval-when-compile
;                        (add-to-list 'load-path
;                                     (expand-file-name "~/.guix-home/profile/share/emacs/site-lisp/use-package-2.4.1"))
;                        (require 'use-package))
;                      (use-package evil
;                                   :init
;                                   (setq evil-want-integration t)
;                                   (setq evil-want-keybinding nil)
;                                   (setq evil-want-C-u-scroll t)
;                                   (setq evil-want-C-i-jump nil)
;                                   :hook (evil-mode . rune/evil-hook)
;                                   :config
;                                   (evil-mode 1)
;                                   (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
;                                   (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join))
;                      ; (use-package doom-modeline
;                      ;              :ensure t
;                      ;              :init (doom-modeline-mode 1)
;                      ;              :custom ((doom-modeline-height 15)))
;                      ))))
