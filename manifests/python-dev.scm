(define-module (manifests python-dev)
               #:use-module (gnu packages))

(specifications->manifest
  (list
    "python-twine"
    "python-wheel"
    "python-keyring"
    ))
