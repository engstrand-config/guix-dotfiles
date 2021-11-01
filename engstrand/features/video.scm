(define-module (engstrand features video)
               #:use-module (rde features)
               #:use-module (rde features predicates)
               #:use-module (gnu services)
               #:use-module (gnu home services)
               #:use-module (engstrand utils)
               #:export (
                         ; feature-mpv
                         feature-obs))
                         ; %engstrand-mpv-configuration))

; The mpv home service seems to have been removed.
; Allows each user to easily extend a shared default configuration
; (define %engstrand-mpv-configuration
;   (home-mpv-configuration))

; (define* (feature-mpv
;            #:key
;            (mpv-configuration %engstrand-mpv-configuration))
;          "Setup the mpv video player."

;          (define (get-home-services config)
;            "Return a list of home services required by mpv."
;            (list
;              ; home-mpv-service-type seems to have been removed from guix home.
;              ; How can we access it now?
;              (service home-mpv-service-type mpv-configuration)))

;          (feature
;            (name 'mpv)
;            (home-services-getter get-home-services)))

(define* (feature-obs
           #:key
           (wayland? #t))
           "Setup OBS, the Open Broadcaster Software."

           (define (get-home-services config)
             "Return a list of home services required by OBS."
             (list
               (simple-service
                 'add-obs-home-packages-to-profile
                 home-profile-service-type
                 (let ((base-packages '("obs")))
                   (pkgs (if wayland?
                             (cons "obs-wlrobs" base-packages)
                             base-packages))))))

           (feature
             (name 'obs)
             (home-services-getter get-home-services)))
