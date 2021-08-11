(define-module (engstrand packages wayland)
               #:use-module (guix packages)
               #:use-module (guix git-download)
               #:use-module (guix build-system meson)
               #:use-module (gnu packages)
               #:use-module (gnu packages wm)
               #:use-module (gnu packages gcc)
               #:use-module (gnu packages man)
               #:use-module (gnu packages image)
               #:use-module (gnu packages xdisorg)
               #:use-module (gnu packages pkg-config)
               #:use-module (gnu packages build-tools)
               #:use-module (gnu packages freedesktop)
               #:use-module (gnu packages datastructures)
               #:use-module ((guix licenses) #:prefix license:))

(define-public wbg
               (package
                 (name "wbg")
                 (version "1.0.2")
                 (source
                   (origin
                     (method git-fetch)
                     (uri (git-reference
                            (url "https://codeberg.org/dnkl/wbg")
                            (commit version)))
                     (file-name (git-file-name name version))
                     (sha256
                       (base32
                         "182cyp97lxwxl5r6f25irvm62ii0j1knmpwlpwa1w00j2xchx89w"))))
                 (build-system meson-build-system)
                 (arguments
                   `(#:meson ,meson-0.55
                     #:build-type "release"))
                 (native-inputs
                   `(("pkg-config" ,pkg-config)
                     ("wayland-protocols" ,wayland-protocols)
                     ("gcc" ,gcc-10)
                     ("tllist" ,tllist)))
                 (inputs
                   `(("wlroots" ,wlroots)
                     ("wayland" ,wayland)
                     ("pixman" ,pixman)
                     ("libpng" ,libpng)
                     ("libjpeg" ,libjpeg-turbo)))
                 (license license:expat)
                 (home-page "https://codeberg.org/dnkl/wbg")
                 (synopsis "Super simple wallpaper application for Wayland compositors")
                 (description "Super simple wallpaper application for
                              Wayland compositors implementing the layer-shell protocol.")))

(define-public wlsunset
               (package
                 (name "wlsunset")
                 (version "0.2.0")
                 (source
                   (origin
                     (method git-fetch)
                     (uri (git-reference
                            (url "https://git.sr.ht/~kennylevinsen/wlsunset")
                            (commit version)))
                     (file-name (git-file-name name version))
                     (sha256
                       (base32
                         "0hhsddh3rs066rbsjksr8kcwg8lvglbvs67dq0r5wx5c1xcwb51w"))))
                 (build-system meson-build-system)
                 (native-inputs
                   `(("pkg-config" ,pkg-config)
                     ("wayland-protocols" ,wayland-protocols)
                     ("scdoc" ,scdoc)))
                 (inputs
                   `(("wayland" ,wayland)
                     ("gcc" ,gcc-10)))
                 (license license:expat)
                 (home-page "https://git.sr.ht/~kennylevinsen/wlsunset")
                 (synopsis "Day/night gamma adjustments for Wayland compositors")
                 (description "Day/night gamma adjustments for Wayland compositors
                              supporting wlr-gamma-control-unstable-v1.")))
