(define-module (engstrand packages tuxedo-keyboard-module)
    #:use-module (gnu packages tls)
    #:use-module (guix gexp)
    #:use-module (guix utils)
    #:use-module (guix packages)
    #:use-module (guix download)
    #:use-module (guix git-download)
    #:use-module (guix build-system trivial)
    #:use-module (guix build-system linux-module)
    #:use-module ((guix licenses) #:prefix license:))

; https://notabug.org/jlicht/guix-pkgs/src/master/staging/linux-nonfree.scm
(define-public (tuxedo-keyboard-module kernel)
  (package
    (name "tuxedo-keyboard-module")
    (version "3.0.8")
    (source (origin
              (method git-fetch)
              (uri
               (git-reference
                (url "https://github.com/tuxedocomputers/tuxedo-keyboard.git")
                (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1rv3ns4n61v18cpnp36zi47jpnqhj410yzi8b307ghiyriapbijv"))))
    (build-system linux-module-build-system)
    (arguments
     `(#:tests? #f
       #:linux ,kernel))
    (home-page "https://github.com/tuxedocomputers/tuxedo-keyboard")
    (synopsis "Tuxedo computers kernel module for keyboard
backlighting")
    (description "Tuxedo computer kernel module for keyboard
backlighting.")
    (license license:gpl2)))
