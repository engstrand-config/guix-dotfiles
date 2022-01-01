(define-module (engstrand packages xorg)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (gnu packages)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages mpd)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages compton)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages pkg-config))

(define-public engstrand-dwm
  (package
   (inherit dwm)
   (name "engstrand-dwm")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/engstrand-config/dwm.git")
           (commit "86f0361d987dd50f9d85300172920fe556295c6e")))
     (sha256
      (base32 "07clvwvwjxs1gwhnmbd3gq6rzyr0cphxk3ibkzmd53xikrighlm9"))))
   (propagated-inputs
    (list xrdb
          maim
          xclip
          xprop
          xsetroot
          font-awesome
          font-jetbrains-mono
          picom
          xwallpaper))
   (home-page "https://github.com/engstrand-config/dwm")
   (synopsis "Engstrand dwm")))

(define-public engstrand-dmenu
  (package
   (inherit dmenu)
   (name "engstrand-dmenu")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/engstrand-config/dmenu.git")
           (commit "eeb3448e0a76d55b75882bd633adf2b6a4ac2412")))
     (sha256
      (base32 "0lyvrvrllq2k7y21n9hh1dn7vj9iv9682hynvs6kn71lcz0aakga"))))
   (arguments
    `(#:validate-runpath? #f ; skip validation
                          #:tests? #f            ; no tests
                          #:make-flags
                          (list (string-append "CC=" ,(cc-for-target))
                                (string-append "PREFIX=" %output)
                                (string-append "FREETYPEINC="
                                               (assoc-ref %build-inputs "freetype")
                                               "/include/freetype2"))
                          #:phases
                          (modify-phases %standard-phases (delete 'configure))))
   (propagated-inputs
    (list engstrand-dwm))
   (home-page "https://github.com/engstrand-config/dmenu")
   (synopsis "Engstrand dmenu")))

(define-public engstrand-dsblocks
  (package
   (inherit slstatus)
   (name "engstrand-dsblocks")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/engstrand-config/dsblocks.git")
           (commit "404d436455f8ab1de058d7c94363556dbdada0d6")))
     (sha256
      (base32 "1kb9an9bmfg9vms4jkz8z400ydrs72plz6rqaakrrfh3in6msjyc"))))
   (arguments
    `(#:tests? #f                      ; no tests
               #:make-flags
               (list (string-append "CC=" ,(cc-for-target))
                     (string-append "PREFIX=" %output)
                     (string-append "FREETYPEINC="
                                    (assoc-ref %build-inputs "freetype")
                                    "/include/freetype2"))
               #:phases
               (modify-phases %standard-phases (delete 'configure))))
   (native-inputs
    (list pkg-config))
   (inputs
    (list libx11 libxft libxinerama freetype))
   (propagated-inputs
    (list mpd glib kdeconnect engstrand-dwm libmpdclient))
   (home-page "https://github.com/engstrand-config/dsblocks")
   (description "Custom statusbar for dwm with blocks written in C. Based on dwmblocks")
   (synopsis "Engstrand dsblocks")))

(define-public engstrand-st
  (package
   (inherit st)
   (name "engstrand-st")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/engstrand-config/st.git")
           (commit "ff7964bbf74e249d7c2f2325fc2fcc103fc8db3b")))
     (sha256
      (base32 "127wxailsfqjlycjad7jaxx1ib4655k3w6c03fc7q3q8y9fd7j4x"))))
   (home-page "https://github.com/engstrand-config/st")
   (inputs
    (list libx11 libxft freetype harfbuzz))
   (propagated-inputs
    (list engstrand-dwm))
   (synopsis "Engstrand st")))
