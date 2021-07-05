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
            (simple-service
                'dotfiles home-files-service-type
                (list `("aliasrc" ,(local-file "files/aliasrc")))))))
