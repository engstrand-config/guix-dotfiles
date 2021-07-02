(define dunst
  (make <service>
    #:provides '(dunst)
    #:docstring "Run the `dunst' notification daemon."
    #:start (make-forkexec-constructor
              '("dunst")
              #:log-file (string-append (getenv "HOME")
                                        "/log/dunst.log"))
    #:stop (make-kill-destructor)
    #:respawn? #t))
(register-services dunst)

(start dunst)
