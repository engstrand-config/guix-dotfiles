(define-module (engstrand packages browsers)
               #:use-module (guix gexp)
               #:use-module (guix utils)
               #:use-module (guix download)
               #:use-module (guix packages)
               #:use-module (nongnu packages mozilla)
               #:use-module (gnu packages web-browsers))

;; This package is the same as the default qutebrowser package in Guix,
;; but adds another phase that copies all official userscripts into share/.
;;
;; You can find all available userscripts here:
;; @url{https://github.com/qutebrowser/qutebrowser/tree/master/misc/userscripts}.
(define-public
  qutebrowser-with-scripts
  (package
    (inherit qutebrowser)
    (name "qutebrowser-with-scripts")
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

;; Pin Firefox to 95.0.2 to prevent extremey lengthy Firefox compiles.
;; It should be enough to update to a new version every few months or so.
;; We still want other updated packages from gnu and nongnu channels,
;; hence why they are not pinned.
(define-public
  firefox/wayland-95.0.2
  (package
    (inherit firefox/wayland)
    (version "95.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://archive.mozilla.org/pub/firefox/releases/"
                           version "/source/firefox-" version ".source.tar.xz"))
       (sha256
        (base32 "0r2dwymgrv25yz8jlsgdmmxs77880dp9r859val3kgbr37vcny61"))))))
