(define-module (engstrand features wayland))

;; rewrite with match
(define (transform-bemenu-options lst)
  (define (make-cli-argument config-pair)
    (let ((argument (car config-pair)) (value (cdr config-pair)))
      (if (not value) ""
          (string-append "--" argument
                         (cond ((eq? value #t) "")
                               ((string? value) (string-append " " "'" value "'"))
                               ((number? value) (string-append " " (number->string value)))
                               (else (raise "invalid bemenu argument!")))))))
  (string-join (map make-cli-argument lst)))

; TODO: Add packages:
;       (simple-service
;         'desktop-environment home-profile-service-type
;         (map specification->package
;              '("bemenu" "foot" "bemenu"))))

; TODO: Add service
;       (simple-service
;         'bemenu-options home-environment-variables-service-type
;         `(("BEMENU_OPTS" . ,(string-append
;                               "\""
;                               (transform-bemenu-options
;                                 '(("ignorecase" . #t)
;                                   ("line-height" . 21)
;                                   ("filter" . #f)
;                                   ("wrap" . #f)
;                                   ("list" . #f)
;                                   ("prompt" . #f)
;                                   ("prefix" . #f)
;                                   ("index" . #f)
;                                   ("password" . #f)
;                                   ("scrollbar" . #f)
;                                   ("ifne" . #f)
;                                   ("fork" . #f)
;                                   ("no-exec" . #f)
;                                   ("bottom" . #f)
;                                   ("grab" . #f)
;                                   ("no-overlap" . #f)
;                                   ("monitor" . #f)
;                                   ("line-height" . 0)
;                                   ("fn" . "'JetBrains Mono 10'")
;                                   ("tb" . #f)
;                                   ("tf" . #f)
;                                   ("fb" . #f)
;                                   ("ff" . #f)
;                                   ("nb" . #f)
;                                   ("nf" . #f)
;                                   ("hb" . #f)
;                                   ("hf" . #f)
;                                   ("sb" . #f)
;                                   ("sf" . #f)
;                                   ("scb" . #f)
;                                   ("scf" . #f)))
;                               "\""))))

; TODO: Add service:
;       (service home-dwl-guile-service-type
;                (home-dwl-guile-configuration
;                  (patches (list ;%patch-xwayland ; causes issues with firefox
;                                 %patch-alpha
;                                 %patch-focusmon
;                                 %patch-vanitygaps
;                                 %patch-attachabove))
;                  (config
;                    (dwl-config
;                      (terminal '("foot"))
;                      (natural-scrolling #t)
;                      (xkb-rules %keyboard-layout)
;                      (colors
;                        (dwl-colors
;                          (root '(0 0 1 1))))))))
