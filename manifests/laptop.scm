(define-module (manifests laptop)
               #:use-module (gnu packages))

(specifications->manifest
  (list
    "tlp" ;; should be required by the laptop module
    "brightnessctl" ;; should be required by the laptop module
    ))
