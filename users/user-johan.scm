(define-module (users user-johan)
    #:use-module (users user-base))

(define-public johan
    (system-user
      (account "johan")
      (name "Johan Engstrand")
      (email "johan@engstrand.nu")
      (github "johanengstrand")
      (gpg-key "DFC6C6B70EF5F7CB75EE97E6DF3088DDBCAD566D")
      (sign-commits? #t)))
