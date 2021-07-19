(define-module (users user-fredrik)
    #:use-module (users user-base))

(define-public fredrik
    (system-user
      (account "fredrik")
      (name "Fredrik Engstrand")
      (email "fredrik@engstrand.nu")
      (github "frewacom")
      (gpg-key "C9BEB8A04458FDDF12681B39029D8EB77E18D68C")
      (sign-commits? #t)))
