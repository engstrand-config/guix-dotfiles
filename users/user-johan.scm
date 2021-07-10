(define-module (users user-johan)
    #:use-module (users user-base))

(define-public johan
    (make-system-user
        "johan"
        "Johan Engstrand"
        "johan@engstrand.nu"
        "123"
        "johanengstrand"))
