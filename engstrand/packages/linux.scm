(define-module (engstrand packages linux)
  #:use-module (gnu packages tls)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system linux-module)
  #:use-module ((guix licenses) #:prefix license:))

(define-public (tuxedo-keyboard-module kernel)
  (package
   (name "tuxedo-keyboard-module")
   (version "3.2.1")
   (source (origin
            (method git-fetch)
            (uri
             (git-reference
              (url "https://github.com/tuxedocomputers/tuxedo-keyboard.git")
              (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "13fncirqcci46zxmsrawsxpazip5k46i849dwkhkqlg0mg4vxxw5"))))
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
