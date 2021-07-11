(define-module (manifests filesharing-downloads)
               #:use-module (gnu packages))

(specifications->manifest
  (list
    "youtube-dl"
    "qbittorrent"
    ))
