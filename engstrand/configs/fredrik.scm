(define-module (engstrand configs fredrik)
               #:use-module (rde features)
               #:use-module (rde features ssh)
               #:use-module (rde features base)
               #:use-module (rde features gnupg)
               #:use-module (gnu services)
               #:use-module (gnu services databases)
               #:use-module (gnu home-services ssh) ;; rde home-service
               #:use-module (dtao-guile home-service)
               #:use-module (engstrand utils)
               #:use-module (engstrand configs)
               #:use-module (engstrand features xorg)
               #:use-module (engstrand features sync)
               #:use-module (engstrand features utils)
               #:use-module (engstrand features state)
               #:use-module (engstrand features emacs)
               #:use-module (engstrand features browsers)
               #:use-module (engstrand features virtualization)
               #:use-module (engstrand features wayland))

(define-public %user-features
               (append
                 (list
                   (feature-user-info
                     #:user-name "fredrik"
                     #:full-name "Fredrik Engstrand"
                     #:email "fredrik@engstrand.nu")
                   ;; TODO: Add custom gnupg feature. We should be using pinentry-bemenu instead.
                   ;;       However, home-gnupg-service will always append "/bin/pinentry" to the
                   ;;       pinentry exectuable in ~/.gnupg/gpg-agent.conf. pinentry-bemenu does not
                   ;;       have such an exectuable. The bemenu exectuable is called "pinentry-bemenu".
                   ;;
                   ;;       This leaves us with two options; add support for it in the home-service,
                   ;;       or transform the pinentry-bemenu package and move the pinentry exectuable to "/bin/pinentry".
                   (feature-gnupg
                     #:gpg-primary-key "C9BEB8A04458FDDF12681B39029D8EB77E18D68C"
                     #:pinentry-flavor 'gtk2
                     #:gpg-smart-card? #f)
                   (feature-ssh
                     #:ssh-configuration
                     (home-ssh-configuration
                       (extra-config
                         (list
                           (ssh-host
                             (host "aur.archlinux.org")
                             (options `((identity-file . "~/.ssh/aur")
                                        (user . "aur"))))))))
                   ;; (feature-state-git
                   ;;   #:repos
                   ;;   `(("repos/pywalfox" . "git@github.com:frewacom/pywalfox.git")
                   ;;     ("repos/pywalfox-native" . "git@github.com:frewacom/pywalfox-native.git")))
                   (feature-virtualization)
                   (feature-qutebrowser)
                   (feature-firefox
                     #:add-keybindings? #f)
                   (feature-kdeconnect)
                   (feature-bitwarden-cli
                     #:email "frewacom@gmail.com"
                     #:lock-timeout 120)
                   (feature-custom-services
                     #:home-services
                     (list
                       (service home-dtao-guile-service-type
                                (home-dtao-guile-configuration
                                  (config
                                    (dtao-config
                                      (use-dwl-guile-colorscheme? #t)
                                      (background-color "FFFFFF")
                                      (block-spacing 10)
                                      (modules `((ice-9 popen)
                                                 (ice-9 rdelim)
                                                 (ice-9 match)
                                                 (srfi srfi-1)))
                                      (left-blocks
                                        (list
                                          (dtao-block
                                            (events? #t)
                                            (render `(dtao:title)))))
                                      (center-blocks
                                        (list
                                          (dtao-block
                                            (interval 1)
                                            (click `(match
                                                       button
                                                       (0 (system* "notify-send" "button 1"))
                                                       (1 (system* "notify-send" "button 2"))
                                                       (2 (system* "notify-send" "button 3"))
                                                       (_ #f)))
                                            (render
                                              `(let* ((port (open-input-pipe "date \"+%a %d %b (v.%V) %T\""))
                                                      (result (read-line port)))
                                                 (close-pipe port)
                                                 (string-append "^bg(#FF0000)" result "^bg()"))))))
                                      (right-blocks
                                        (list
                                          (dtao-block
                                            (interval 5)
                                            (click `(system* "notify-send" "\"goodbye world\""))
                                            (render
                                              `(let* ((port (open-input-file "/sys/class/power_supply/BAT0/capacity"))
                                                      (result (read-line port)))
                                                 (close-port port)
                                                 (string-append "^fg(#00FF00)"result "%^fg()")))))))))))
                     #:system-services
                     (list
                       (service mysql-service-type
                                (mysql-configuration
                                  (bind-address "0.0.0.0")))))
                   (feature-wayland-wbg
                     #:path (string-append (getenv "HOME")
                                           "/engstrand-config/wallpapers/f1/qlhIf9n.jpg")))
                 %engstrand-emacs-base-features
                 ;; TODO: When Firefox feature is done, add it, but set (add-keybindings? #f)
                 (modify-features %engstrand-base-features
                                  (delete 'ssh))))
