(define-module (engstrand features version-control)
  #:use-module (rde features)
  #:use-module (gnu services)
  #:use-module (gnu home-services version-control)
  #:export (feature-git-colorscheme))

(define* (feature-git-colorscheme)
  "Changes the colorscheme of git in the terminal using farg."

  (define (get-home-services config)
    (list
     (simple-service
      'update-git-colors
      home-git-service-type
      (home-git-extension
       (config
        `((color
           ((ui . auto)))
          ;; Special colors, see foot feature
          (,(string->symbol "color \"diff\"")
           ((old . ,(string->symbol "red dim"))
            (new . ,(string->symbol "green dim"))))
          (,(string->symbol "color \"status\"")
           ((added . ,(string->symbol "green dim"))
            (changed . ,(string->symbol "red dim"))
            (untracked . ,(string->symbol "red dim"))))
          (,(string->symbol "color \"\"")
           ((added . ,(string->symbol "green dim"))
            (changed . ,(string->symbol "red dim"))
            (untracked . ,(string->symbol "red dim"))))))))))

    (feature
     (name 'git-colorscheme)
     (home-services-getter get-home-services)))
