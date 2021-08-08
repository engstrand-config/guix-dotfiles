(define-module (engstrand packages engstrand-st)
    #:use-module (guix packages)
    #:use-module (guix download)
    #:use-module (guix git-download)
    #:use-module (gnu packages)
    #:use-module (gnu packages gtk)
    #:use-module (gnu packages suckless)
    #:use-module (gnu packages fonts)
    #:use-module (gnu packages fontutils)
    #:use-module (gnu packages xorg)
    #:use-module (engstrand packages)
    #:use-module (engstrand packages engstrand-dwm))

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
     	    `(("libx11" ,libx11)
       	      ("libxft" ,libxft)
	      ("freetype" ,freetype)
	      ("harfbuzz" ,harfbuzz)))
        (propagated-inputs
             `(("engstrand-dwm", engstrand-dwm)))
	(synopsis "Engstrand st")))
