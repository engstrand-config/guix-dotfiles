(define-module (engstrand utils))

; Converts a list of kernel modules into a list of packages.
; Each kernel module should accept the current system kernel
; as a single argument. The kernel module should then dynamically
; create a valid kernel module package based on the specified kernel.
(define-public (kernel-modules->list modules kernel)
               (map (lambda (mod) (mod kernel)) modules))
