(define-module (engstrand packages python-pywalfox)
               #:use-module (guix packages)
               #:use-module (guix download)
               #:use-module (guix build-system python)
               #:use-module (guix licenses)
               #:use-module (engstrand packages))

(define-public python-pywalfox
  (package
    (name "python-pywalfox")
    (version "2.7.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pywalfox" version))
        (sha256
          (base32
            "1gkxcnysygvcpfhinaxaa6lf7b7194x7vi928i0qpdw962ck1zsi"))))
    (build-system python-build-system)
    (home-page
      "https://github.com/frewacom/pywalfox")
    (synopsis
      "Native app used alongside the Pywalfox browser extension")
    (description
      "Native app used alongside the Pywalfox browser extension")
    (license #f)))
