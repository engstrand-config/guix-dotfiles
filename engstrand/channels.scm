(define-module (engstrand channels)
               #:use-module (guix channels))

(list
  (channel
    (name 'guix)
    (url "https://git.savannah.gnu.org/git/guix.git")
    (introduction
      (make-channel-introduction
        "9edb3f66fd807b096b48283debdcddccfea34bad"
        (openpgp-fingerprint
          "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
  (channel
    (name 'engstrand-config)
    (url "https://github.com/engstrand-config/guix-dotfiles")
    (branch "channel")
    (introduction
      (make-channel-introduction
        "005c42a980c895e0853b821494534d67c7b85e91"
        (openpgp-fingerprint
          "C9BE B8A0 4458 FDDF 1268 1B39 029D 8EB7 7E18 D68C")))))
