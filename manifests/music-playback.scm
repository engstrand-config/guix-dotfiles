(define-module (manifests music-playback)
               #:use-module (gnu packages))

(specifications->manifest
  (list
    "mpc"
    "libmpdclient"
    "mpd"
    ))

