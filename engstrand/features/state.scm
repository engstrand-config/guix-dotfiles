(define-module (engstrand features state)
               #:use-module (rde features)
               #:use-module (rde features predicates)
               #:use-module (gnu services)
               #:use-module (gnu services nix)
               #:use-module (gnu home-services state)
               #:use-module (gnu home-services files)
               #:use-module (engstrand utils)
               #:export (
                         feature-dotfiles
                         feature-state-git
                         feature-state-rsync))

(define* (feature-dotfiles
           #:key
           (dotfiles '()))
         "Symlink dotfiles to home."

         (ensure-pred list-of-dotfiles? dotfiles)

         (define (get-home-services config)
           "Return a list of home services required for adding dotfiles."
           (list
             (simple-service
               'add-dotfiles-to-symlink
               home-files-service-type
               dotfiles)))

         (feature
           (name 'dotfiles)
           (home-services-getter get-home-services)))

(define* (feature-state-git
           #:key
           (prefix (getenv "HOME"))
           (repos '()))
         "Add git repository states that can be synced using shepherd."

         (ensure-pred string? prefix)
         (ensure-pred list-of-state-items? repos)

         (define (get-home-services config)
           "Return a list of home services required for adding git states."
           (list
             (simple-service
               'add-state-git-repos
               home-state-service-type
               (map (lambda (repo)
                      (state-git
                        (string-append prefix "/" (car repo))
                        (cdr repo)))
                    repos))))

         (feature
           (name 'state-git)
           (home-services-getter get-home-services)))

(define* (feature-state-rsync
           #:key
           (prefix (getenv "HOME"))
           (hosts '()))
         "Add rsync states that can be synces using shepherd."

         (ensure-pred string? prefix)
         (ensure-pred list-of-state-items? hosts)

         (define (get-home-services config)
           "Return a list of home services required for adding rsync states."
           (list
             (simple-service
               'add-state-rsync-hosts
               home-state-service-type
               (map (lambda (host)
                      (state-rsync
                        (string-append prefix "/" (car host))
                        (cdr host)))
                    hosts))))

         (feature
           (name 'state-rsync)
           (home-services-getter get-home-services)))
