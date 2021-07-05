(define-module (home engstrand)
                #:use-module (gnu home)
                #:use-module (gnu services)
                #:use-module (gnu packages fonts)
                #:use-module (guix gexp)
                #:use-module (gnu home-services)
                #:use-module (gnu home-services files)
                #:use-module (gnu home-services-utils)
                #:use-module (gnu home-services shells)
                #:use-module (gnu home-services ssh)
                #:use-module (gnu home-services xdg)
                #:use-module (gnu home-services version-control))


(home-environment
    (services
        (list
            (service home-ssh-service-type)
            (service home-bash-service-type
                (home-bash-configuration
                    (guix-defaults? #t)
                    (bash-profile `(,(slurp-file-gexp (local-file "files/shell-profile"))))
                    (bashrc `(,(slurp-file-gexp (local-file "files/bashrc"))))))
            (service home-xdg-user-directories-service-type
                (home-xdg-user-directories-configuration
                    (download "$HOME/downloads")
                    (documents "$HOME/documents")
                    (pictures "$HOME/images")
                    (music "$HOME/music")
                    (videos "$HOME")
                    (publicshare "$HOME")
                    (templates "$HOME")
                    (desktop "$HOME")))
            (simple-service
                'dotfiles home-files-service-type
                (list
                    `("aliasrc" ,(local-file "files/aliasrc"))
                    `("inputrc" ,(local-file "files/inputrc"))
                    `("xmodmap" ,(local-file "files/xmodmap"))
                    `("xsession" ,(local-file "files/xsession"))
                    `("nix-channels" ,(local-file "files/nix-channels"))
                    `("config/guix/channels.scm" ,(local-file "../channels.scm"))
                    `("config/dunst/dunstrc" ,(local-file "files/config/dunst/dunstrc"))
                    `("config/nvim/init.vim" ,(local-file "files/config/nvim/init.vim"))
                    `("config/nvim/autoload/plug.vim" ,(local-file "files/config/nvim/autoload/plug.vim"))
                    `("config/picom/picom.conf" ,(local-file "files/config/picom/picom.conf")))))))
