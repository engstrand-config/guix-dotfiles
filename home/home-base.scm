(define-module (home home-base)
               #:use-module (users user-base)
               #:use-module (gnu home)
               #:use-module (gnu services)
               #:use-module (gnu packages)
               #:use-module (gnu packages xorg)
               #:use-module (gnu packages gnupg)
               #:use-module (gnu packages fonts)
               #:use-module (gnu packages xdisorg)
               #:use-module (engstrand packages)
               #:use-module (engstrand packages engstrand-utils)
               #:use-module (srfi srfi-98) ; for get-environment-variable
               #:use-module (ice-9 exceptions) ; for exceptions
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
               #:export (base-home-environment))

(define (abspath homedir path) (string-append homedir "/" path))

(define* (base-home-environment
           #:key
           (user '())
           (home (get-environment-variable "HOME"))
           (packages '())
           (services '())
           (repos '())
           (rsync '())
           (dotfiles '()))
         (if (not (system-user? user)) (throw 'invalid-user . (display "Invalid user argument, expected user record")))
         (home-environment
           (packages
             (map specification->package
                  (append
                    '("ncurses"
                      "gnupg"
                      "pinentry"
                      "htop"
                      "sxiv"
                      "mpv"
                      "zsh")
                    packages)))
           (services
             (append
               (list
                 (service home-ssh-service-type)
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
                                     '("engstrand-config/st" ,"git@github.com:engstrand-config/st.git")
                                     '("engstrand-config/dwm" ,"git@github.com:engstrand-config/dwm.git")
                                     '("engstrand-config/dmenu" ,"git@github.com:engstrand-config/dmenu.git")
                                     '("engstrand-config/utils" ,"git@github.com:engstrand-config/utils.git")
                                     '("engstrand-config/dsblocks" ,"git@github.com:engstrand-config/dsblocks.git")
                                     '("engstrand-config/guix-channel" ,"git@github.com:engstrand-config/guix-channel.git"))
                                   repos))))
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
                 (simple-service
                   'bootstrap home-run-on-first-login-service-type
                   #~(system* #$(file-append engstrand-utils "/bin/bootstrap"))))
               services))))
