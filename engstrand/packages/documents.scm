(define-module (engstrand packages documents)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system qt)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages web)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages image)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages javascript))

(define-public sioyek
  (package
    (name "sioyek")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ahrm/sioyek")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vmmp2s032ygh1byz77pg9aljmp8hx745fr7mmz11831f96mlmhq"))))
    (inputs
     (list gumbo-parser
           freetype
           harfbuzz
           jbig2dec
           mujs
           mupdf
           openjpeg
           libjpeg-turbo
           qt3d-5
           qtbase-5))
    (build-system qt-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases
         %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (invoke "qmake" (string-append "PREFIX=" out)))))
         (add-after 'unpack 'patch-source-files
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (share (string-append out "/share/sioyek"))
                             (etc (string-append out "/etc/sioyek")))
                        (substitute* "pdf_viewer/main.cpp"
                                     (("/usr/share/sioyek") share)
                                     (("/etc/sioyek") etc))
                        (substitute* "pdf_viewer_build_config.pro"
                                     (("-lmupdf-third ") ""))
                        #t))))))
    (home-page "https://sioyek.info")
    (synopsis "A PDF viewer designed for research papers and technical books")
    (description "Sioyek is a PDF viewer with a focus on technical books and research papers.")
    (license license:gpl3)))

(define-public sioyek/wayland
  (package
   (inherit sioyek)
   (name "sioyek-wayland")
   (inputs
    (modify-inputs (package-inputs sioyek)
                   (append qtwayland-5)))))
