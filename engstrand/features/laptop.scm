(define-module (engstrand features laptop))

; TODO: Add packages
; TODO: Add services
;       (if laptop?
;           (list
;             (udev-rules-service 'backlight %backlight-udev-rule)
;             (service tlp-service-type
;                      (tlp-configuration
;                        (cpu-scaling-governor-on-ac (list "performance"))
;                        (sched-powersave-on-bat? #t))))
;           '())
