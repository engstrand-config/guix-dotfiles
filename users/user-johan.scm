(define-module (users user-johan)
    #:use-module (users user-base))

(define-public johan
    (make-system-user
        "johan"
        "Johan Engstrand"
        "johan@engstrand.nu"
        "johanengstrand"
        "DFC6C6B70EF5F7CB75EE97E6DF3088DDBCAD566D"
        #t))
