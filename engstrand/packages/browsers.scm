(define-module (engstrand packages browsers)
               #:use-module (guix gexp)
               #:use-module (guix utils)
               #:use-module (guix download)
               #:use-module (guix packages)
               #:use-module (gnu packages web-browsers))

; This package is the same as the default qutebrowser package in Guix,
; but adds another phase that copies all official userscripts into share/.
;
;; You can find all available userscripts here:
; @url{https://github.com/qutebrowser/qutebrowser/tree/master/misc/userscripts}.
(define-public
  qutebrowser-with-scripts
  (package
    (inherit qutebrowser)
    (name "qutebrowser-with-scripts")
    (version "2.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/qutebrowser/"
                            "qutebrowser/releases/download/v" version "/"
                            "qutebrowser-" version ".tar.gz"))
        (sha256
          (base32 "05n64mw9lzzxpxr7lhakbkm9ir3x8p0rwk6vbbg01aqg5iaanyj0"))))
    (arguments
      `(,@(substitute-keyword-arguments
            (package-arguments qutebrowser)
            ((#:phases phases)
             `(modify-phases
                ,phases
                (add-after 'install 'install-userscripts
                           (lambda* (#:key outputs #:allow-other-keys)
                                    (let* ((out (assoc-ref outputs "out"))
                                           (scripts (string-append out "/share/qutebrowser/userscripts")))
                                      (mkdir-p scripts)
                                      (copy-recursively "misc/userscripts" scripts)))))))))))
