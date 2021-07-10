(define-module (users user-fredrik)
    #:use-module (users user-base))

(define-public fredrik
    (make-system-user
        "fredrik"
        "Fredrik Engstrand"
        "fredrik@engstrand.nu"
        "C9BEB8A04458FDDF12681B39029D8EB77E18D68C"
        "frewacom"))
