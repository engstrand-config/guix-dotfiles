(define-module (users user-fredrik)
    #:use-module (users user-base))

(define-public fredrik
    (make-system-user
        "fredrik"
        "Fredrik Engstrand"
        "fredrik@engstrand.nu"
        "123"
        "frewacom"))
