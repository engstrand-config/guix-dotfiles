(define-module (engstrand features state))

; TODO: Add feature for symlinking dotfiles
;       (simple-service
;         'dotfiles home-files-service-type
;         (append
;           (list
;             `("aliasrc" ,(local-file "files/aliasrc"))
;             `("inputrc" ,(local-file "files/inputrc"))
;             `("nix-channels" ,(local-file "files/nix-channels"))
;             `("config/guix/channels.scm" ,(local-file "../channels.scm"))
;             `("config/dunst/dunstrc" ,(local-file "files/config/dunst/dunstrc"))
;             `("config/nvim/init.vim" ,(local-file "files/config/nvim/init.vim"))
;             `("config/nvim/autoload/plug.vim" ,(local-file "files/config/nvim/autoload/plug.vim"))
;             `("config/picom/picom.conf" ,(local-file "files/config/picom/picom.conf")))
;           dotfiles))

; TODO: Add feature for adding state
;       (service home-state-service-type
;                (append
;                  (map (lambda (pair) (state-rsync (abspath home (car pair)) (cadr pair))) rsync)
;                  (map (lambda (pair) (state-git (abspath home (car pair)) (cadr pair)))
;                       (append
;                         (list
;                           '("engstrand-config/utils" ,"git@github.com:engstrand-config/utils.git")
;                           '("engstrand-config/guix-channel" ,"git@github.com:engstrand-config/guix-channel.git"))
;                         repos))))
