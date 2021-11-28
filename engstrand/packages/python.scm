(define-module (engstrand packages python)
               #:use-module (gnu packages libffi)
               #:use-module (gnu packages python-xyz)
               #:use-module (guix packages)
               #:use-module (guix download)
               #:use-module (guix build-system python)
               #:use-module ((guix licenses) #:prefix license:))

(define-public
  python-pywalfox
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

(define-public
  python-soundfile
  (package
    (name "python-soundfile")
    (version "0.10.3.post1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "SoundFile" version))
        (sha256
          (base32 "0yqhrfz7xkvqrwdxdx2ydy4h467sk7z3gf984y1x2cq7cm1gy329"))))
    (build-system python-build-system)
    (propagated-inputs `(("python-cffi" ,python-cffi)))
    (home-page "https://github.com/bastibe/PySoundFile")
    (synopsis "An audio library based on libsndfile, CFFI and NumPy")
    (description "An audio library based on libsndfile, CFFI and NumPy")
    (license #f)))

(define-public
  python-sounddevice
  (package
    (name "python-sounddevice")
    (version "0.4.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "sounddevice" version))
        (sha256
          (base32 "083crfkrx0y9gqybqcixzhrp7swqjr78nrmz9r6aqpxncxs7lrpi"))))
    (build-system python-build-system)
    (propagated-inputs `(("python-cffi" ,python-cffi)))
    (home-page "http://python-sounddevice.readthedocs.io/")
    (synopsis "Play and Record Sound with Python")
    (description "Play and Record Sound with Python")
    (license license:expat)))

(define-public
  python-tikzplotlib
  (package
    (name "python-tikzplotlib")
    (version "0.9.15")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "tikzplotlib" version))
        (sha256
          (base32 "0f6cwnb51fnds2x9cycjk5gw5xnp0g3bdv8439xij8y0hijfwjcl"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-importlib-metadata" ,python-importlib-metadata)
        ("python-matplotlib" ,python-matplotlib)
        ("python-numpy" ,python-numpy)
        ("python-pillow" ,python-pillow)))
    (home-page "https://github.com/nschloe/tikzplotlib")
    (synopsis "Convert matplotlib figures into TikZ/PGFPlots")
    (description "Convert matplotlib figures into TikZ/PGFPlots")
    (license license:expat)))
