(define-module (manifests nyxt)
               #:use-module (gnu packages))

; TODO: Move into home service
(specifications->manifest
  (list
    "nyxt"
    ; Add support for media playback
    "gst-plugins-base"
    "gst-plugins-good"
    "gst-plugins-ugly"
    "gst-plugins-bad"
    ))
