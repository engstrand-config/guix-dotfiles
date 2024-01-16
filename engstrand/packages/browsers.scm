(define-module (engstrand packages browsers)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages web-browsers))

;; Until https://issues.guix.gnu.org/63558 is merged.
(define-public qutebrowser/wayland
  (package/inherit
   qutebrowser
   (name "qutebrowser-wayland")
   (inputs
    (modify-inputs (package-inputs qutebrowser)
                   (prepend qtwayland)))
   (arguments
    (substitute-keyword-arguments
     (package-arguments qutebrowser)
     ((#:phases phases)
      #~(modify-phases
         #$phases
         (add-after 'wrap-qt-process-path 'wrap-qtwebengine-path
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (wrap-program (search-input-file outputs "bin/qutebrowser")
                                    `("QT_PLUGIN_PATH" =
                                      (,(string-append (assoc-ref inputs "qtwayland")
                                                       "/lib/qt6/plugins/"))))))))))))
