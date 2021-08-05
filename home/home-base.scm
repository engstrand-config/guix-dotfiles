(define-module (home home-base)
               #:use-module (srfi srfi-1)
               #:use-module (ice-9 match)
               #:use-module (users user-base)
               #:use-module (gnu home)
               #:use-module (gnu services)
               #:use-module (gnu packages)
               #:use-module (gnu packages xorg)
               #:use-module (gnu packages gnupg)
               #:use-module (gnu packages fonts)
               #:use-module (gnu packages xdisorg)
               #:use-module (gnu packages base)
               #:use-module (engstrand packages)
               #:use-module (systems system-base)
               #:use-module (engstrand packages engstrand-utils)
               #:use-module (ice-9 exceptions)
               #:use-module (guix gexp)
               #:use-module (gnu home-services)
               #:use-module (gnu home-services files)
               #:use-module (gnu home-services-utils)
               #:use-module (gnu home-services shells)
               #:use-module (gnu home-services shellutils)
               #:use-module (gnu home-services state)
               #:use-module (gnu home-services ssh)
               #:use-module (gnu home-services xdg)
               #:use-module (gnu home-services version-control)
               #:use-module (gnu home-services video)
               #:use-module (gnu home-services emacs)
               #:use-module (dwl-guile patches)
               #:use-module (dwl-guile home-service)
               #:use-module (gnu packages emacs-xyz)
               #:use-module (flat packages emacs)
               #:use-module (gnu packages emacs)
               #:export (base-home-environment))

(define (abspath homedir path) (string-append homedir "/" path))

;; rewrite with match
(define (transform-bemenu-options lst)
  (define (make-cli-argument config-pair)
    (let ((argument (car config-pair)) (value (cdr config-pair)))
      (if (not value) ""
          (string-append "--" argument
                         (cond ((eq? value #t) "")
                               ((string? value) (string-append " " "'" value "'"))
                               ((number? value) (string-append " " (number->string value)))
                               (else (raise "invalid bemenu argument!")))))))
  (string-join (map make-cli-argument lst)))

(define* (base-home-environment
           #:key
           (user '())
           (home (getenv "HOME"))
           (packages '())
           (services '())
           (repos '())
           (rsync '())
           (mpv-extra-config '())
           (dotfiles '()))
         (if (not (system-user? user)) (throw 'invalid-user . (display "Invalid user argument, expected user record")))
         (home-environment
           (packages
             (map specification->package
                  (append
                    '("ncurses" "gnupg" "pinentry" "zsh")
                    packages)))
           (services
             (append
               (list
                 (service home-ssh-service-type)
                 (service home-dwl-guile-service-type
                          (home-dwl-guile-configuration
                            (patches (list %patch-xwayland
                                           %patch-alpha
                                           %patch-focusmon
                                           %patch-vanitygaps
                                           %patch-attachabove))
                            (config
                              (dwl-config
                                (terminal '("foot"))
                                (natural-scrolling? #t)
                                (xkb-rules %keyboard-layout)
                                (colors
                                  (dwl-colors
                                    (root '(0 0 1 1))))))))
                 (service home-zsh-autosuggestions-service-type)
                 (service home-zsh-service-type
                          (home-zsh-configuration
                            (zshrc `(,(slurp-file-gexp (local-file "files/zshrc"))))
                            (zprofile `(,(slurp-file-gexp (local-file "files/shell-profile"))))))
                 (service home-xdg-user-directories-service-type
                          (home-xdg-user-directories-configuration
                            (download (abspath home "downloads"))
                            (documents (abspath home "documents"))
                            (pictures (abspath home "images"))
                            (music (abspath home "music"))
                            (videos (abspath home "videos"))
                            (publicshare home)
                            (templates home)
                            (desktop home)))
                 (service home-git-service-type
                          (home-git-configuration
                            (config
                              (let ((%use-gpg (system-user-sign-commits? user)))
                                `((user
                                    ((name . ,(system-user-name user))
                                     (email . ,(system-user-email user))
                                     ,@(if %use-gpg
                                           `((signingkey . ,(system-user-gpg-key user)))
                                           '())))
                                  (gpg
                                    ((program . ,(file-append gnupg "/bin/gpg"))))
                                  (commit
                                    ((gpgsign . ,%use-gpg)))
                                  (tag
                                    ((gpgsign . ,%use-gpg)))
                                  (pull
                                    ((rebase . ,%use-gpg)))
                                  (github
                                    ((user . ,(system-user-github user)))))))))
                 (service home-state-service-type
                          (append
                            (map (lambda (pair) (state-rsync (abspath home (car pair)) (cadr pair))) rsync)
                            (map (lambda (pair) (state-git (abspath home (car pair)) (cadr pair)))
                                 (append
                                   (list
                                     '("engstrand-config/utils" ,"git@github.com:engstrand-config/utils.git")
                                     '("engstrand-config/guix-channel" ,"git@github.com:engstrand-config/guix-channel.git"))
                                   repos))))
                 (simple-service
                   'desktop-environment home-profile-service-type
                   (map specification->package
                        (append
                          '("dwl" "foot" "bemenu" "htop" "kdeconnect" ))))
                 (simple-service
                   'bemenu-options home-environment-variables-service-type
                   `(("BEMENU_OPTS" . ,(string-append
                                         "\""
                                         (transform-bemenu-options
                                           '(("ignorecase" . #t)
                                             ("line-height" . 21)
                                             ("filter" . #f)
                                             ("wrap" . #f)
                                             ("list" . #f)
                                             ("prompt" . #f)
                                             ("prefix" . #f)
                                             ("index" . #f)
                                             ("password" . #f)
                                             ("scrollbar" . #f)
                                             ("ifne" . #f)
                                             ("fork" . #f)
                                             ("no-exec" . #f)
                                             ("bottom" . #f)
                                             ("grab" . #f)
                                             ("no-overlap" . #f)
                                             ("monitor" . #f)
                                             ("line-height" . 0)
                                             ("fn" . "'JetBrains Mono 10'")
                                             ("tb" . #f)
                                             ("tf" . #f)
                                             ("fb" . #f)
                                             ("ff" . #f)
                                             ("nb" . #f)
                                             ("nf" . #f)
                                             ("hb" . #f)
                                             ("hf" . #f)
                                             ("sb" . #f)
                                             ("sf" . #f)
                                             ("scb" . #f)
                                             ("scf" . #f)))
                                         "\""))))
                 (simple-service
                   'dotfiles home-files-service-type
                   (append
                     (list
                       `("aliasrc" ,(local-file "files/aliasrc"))
                       `("inputrc" ,(local-file "files/inputrc"))
                       `("nix-channels" ,(local-file "files/nix-channels"))
                       `("config/guix/channels.scm" ,(local-file "../channels.scm"))
                       `("config/dunst/dunstrc" ,(local-file "files/config/dunst/dunstrc"))
                       `("config/nvim/init.vim" ,(local-file "files/config/nvim/init.vim"))
                       `("config/nvim/autoload/plug.vim" ,(local-file "files/config/nvim/autoload/plug.vim"))
                       `("config/picom/picom.conf" ,(local-file "files/config/picom/picom.conf")))
                     dotfiles))
                 (service home-mpv-service-type
                          (home-mpv-configuration))
                 (service home-emacs-service-type
                          (home-emacs-configuration
                            (package emacs-pgtk-native-comp)
                            ; (rebuild-elisp-packages? #t)
                            (xdg-flavor? #t)
                            (server-mode? #t)
                            (elisp-packages
                              (list
                                emacs-use-package
                                emacs-evil
                                emacs-guix
                                emacs-doom-modeline
                                emacs-geiser
                                emacs-geiser-guile
                                ))
                            (init-el
                              '(
                                (package-initialize)
                                (eval-when-compile
                                  (add-to-list 'load-path
                                               (expand-file-name "~/.guix-home/profile/share/emacs/site-lisp/use-package-2.4.1"))
                                  (require 'use-package))
                                (use-package evil
                                             :init
                                             (setq evil-want-integration t)
                                             (setq evil-want-keybinding nil)
                                             (setq evil-want-C-u-scroll t)
                                             (setq evil-want-C-i-jump nil)
                                             :hook (evil-mode . rune/evil-hook)
                                             :config
                                             (evil-mode 1)
                                             (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
                                             (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join))
                                ; (use-package doom-modeline
                                ;              :ensure t
                                ;              :init (doom-modeline-mode 1)
                                ;              :custom ((doom-modeline-height 15)))
                                ))))
                 (simple-service
                   'bootstrap home-run-on-first-login-service-type
                   #~(system* #$(file-append engstrand-utils "/bin/bootstrap"))))
               services))))
