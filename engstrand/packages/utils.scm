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
    #:use-module (engstrand packages rust)
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

(define-public rbw
  (package
    (name "rbw")
    (version "1.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rbw" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0zszp9hvilpikbd66b5zbvspks0spv8dh0yry0sxnc5yqvl2ixnf"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-aes" ,rust-aes-0.7)
         ("rust-anyhow" ,rust-anyhow-1)
         ("rust-arrayvec" ,rust-arrayvec-0.7)
         ("rust-async-trait" ,rust-async-trait-0.1)
         ("rust-base32" ,rust-base32-0.4)
         ("rust-base64" ,rust-base64-0.13)
         ("rust-block-modes" ,rust-block-modes-0.8)
         ("rust-block-padding" ,rust-block-padding-0.2)
         ("rust-daemonize" ,rust-daemonize-0.4)
         ("rust-directories" ,rust-directories-4)
         ("rust-env-logger" ,rust-env-logger-0.9)
         ("rust-hkdf" ,rust-hkdf-0.11)
         ("rust-hmac" ,rust-hmac-0.11)
         ("rust-humantime" ,rust-humantime-2)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-log" ,rust-log-0.4)
         ("rust-nix" ,rust-nix-0.23)
         ("rust-paw" ,rust-paw-1)
         ("rust-pbkdf2" ,rust-pbkdf2-0.9)
         ("rust-percent-encoding" ,rust-percent-encoding-2)
         ("rust-rand" ,rust-rand-0.8)
         ("rust-region" ,rust-region-3)
         ("rust-reqwest" ,rust-reqwest-0.11)
         ("rust-rsa" ,rust-rsa-0.5)
         ("rust-serde" ,rust-serde-1)
         ("rust-serde-json" ,rust-serde-json-1)
         ("rust-serde-path-to-error" ,rust-serde-path-to-error-0.1)
         ("rust-serde-repr" ,rust-serde-repr-0.1)
         ("rust-sha-1" ,rust-sha-1-0.9)
         ("rust-sha2" ,rust-sha2-0.9)
         ("rust-structopt" ,rust-structopt-0.3)
         ("rust-tempfile" ,rust-tempfile-3)
         ("rust-term-size" ,rust-term-size-0.3)
         ("rust-textwrap" ,rust-textwrap-0.11)
         ("rust-thiserror" ,rust-thiserror-1)
         ("rust-tokio" ,rust-tokio-1.12)
         ("rust-totp-lite" ,rust-totp-lite-1)
         ("rust-url" ,rust-url-2)
         ("rust-uuid" ,rust-uuid-0.8)
         ("rust-zeroize" ,rust-zeroize-1.4))))
    (home-page "https://git.tozt.net/rbw")
    (synopsis "Unofficial Bitwarden CLI")
    (description "Unofficial Bitwarden CLI")
    (license license:expat)))
