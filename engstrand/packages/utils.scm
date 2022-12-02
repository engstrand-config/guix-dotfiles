(define-module (engstrand packages utils)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system cargo)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages xdisorg))

(define-public engstrand-utils
  (package
   (name "engstrand-utils")
   (version "1.0.0")
   (description "Engstrand utilities and scripts")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/engstrand-config/utils.git")
           (commit "066f7f0034af3e908b45ac92124f10f78f1d1417")))
     (sha256
      (base32 "14xg4zw90axcqdwrl5l3ixk044f4rsvnk8rd8vgl6mmi0iviq28z"))))
   (build-system copy-build-system)
   (propagated-inputs
    (list xcape
          setxkbmap
          xprop
          xset
          xclip
          xdotool
          maim
          xwallpaper
          xrdb))
   (home-page "https://github.com/engstrand-config/utils")
   (synopsis "Engstrand utilities and scripts")
   (license license:gpl3)))
