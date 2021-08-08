(define-module (engstrand packages engstrand-dwl)
               #:use-module (guix packages)
               #:use-module (guix utils)
               #:use-module (gnu packages)
               #:use-module (gnu packages wm))

(define-public
  engstrand-dwl
  (package
    (inherit dwl)
    (name "engstrand-dwl")
    (source
      (origin
        (inherit (package-source dwl))))
    (arguments
      `(#:tests? #f                      ; no tests
        #:make-flags
        (list
          (string-append "CC=" ,(cc-for-target))
          (string-append "PREFIX=" (assoc-ref %outputs "out")))
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (add-after
            'build
            'install-wayland-session
            (lambda* (#:key outputs #:allow-other-keys)
                     ;; Add a .desktop file to wayland-sessions
                     (let* ((output (assoc-ref outputs "out"))
                            (wayland-sessions (string-append output "/share/wayland-sessions")))
                       (mkdir-p wayland-sessions)
                       (with-output-to-file
                         (string-append wayland-sessions "/dwl.desktop")
                         (lambda _
                           (format #t
                                   "[Desktop Entry]~@
                                   Name=dwl~@
                                   Comment=Dynamic Window Manager for Wayland~@
                                   Exec=~a/bin/dwl~@
                                   TryExec=~@*~a/bin/dwl~@
                                   Icon=~@
                                   Type=Application~%"
                                   output)))))))))))
