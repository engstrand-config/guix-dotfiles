(define-module (home engstrand)
                #:use-module (gnu home)
                #:use-module (gnu services)
                #:use-module (gnu packages xorg)
                #:use-module (gnu packages fonts)
                #:use-module (gnu packages xdisorg)
                #:use-module (engstrand packages)
                #:use-module (engstrand packages engstrand-utils)
                #:use-module (guix gexp)
                #:use-module (gnu home-services)
                #:use-module (gnu home-services files)
                #:use-module (gnu home-services-utils)
                #:use-module (gnu home-services shells)
                #:use-module (gnu home-services state)
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
            (service home-state-service-type
	        (append
	            (list
		        (state-git "$HOME/engstrand-config/st"
                                    "git@github.com:engstrand-config/st.git")
		        (state-git "$HOME/engstrand-config/dwm"
                                    "git@github.com:engstrand-config/dwm.git")
		        (state-git "$HOME/engstrand-config/dmenu"
                                   "git@github.com:engstrand-config/dmenu.git")
		        (state-git "$HOME/engstrand-config/dsblocks"
                                   "git@github.com:engstrand-config/dsblocks.git")
		        (state-git "$HOME/engstrand-config/utils"
                                    "git@github.com:engstrand-config/utils.git")
		        (state-git "$HOME/engstrand-config/wallpapers"
                                   "git@github.com:engstrand-config/wallpapers.git")
		        (state-git "$HOME/engstrand-config/guix-channel"
                                   "git@github.com:engstrand-config/guix-channel.git"))))
            (simple-service
                'dotfiles home-files-service-type
                (list
                    `("aliasrc" ,(local-file "files/aliasrc"))
                    `("inputrc" ,(local-file "files/inputrc"))
                    `("nix-channels" ,(local-file "files/nix-channels"))
                    `("config/guix/channels.scm" ,(local-file "../channels.scm"))
                    `("config/dunst/dunstrc" ,(local-file "files/config/dunst/dunstrc"))
                    `("config/nvim/init.vim" ,(local-file "files/config/nvim/init.vim"))
                    `("config/nvim/autoload/plug.vim" ,(local-file "files/config/nvim/autoload/plug.vim"))
                    `("config/picom/picom.conf" ,(local-file "files/config/picom/picom.conf"))))
            (simple-service
                'bootstrap home-run-on-first-login-service-type
                #~(system* #$(file-append engstrand-utils "/bin/bootstrap"))))))
