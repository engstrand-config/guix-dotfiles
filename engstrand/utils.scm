(define-module (engstrand utils)
               #:use-module (srfi srfi-1)
               #:use-module (guix gexp)
               #:use-module (gnu packages)
               #:use-module (rde features)
               #:use-module (rde features predicates)
               #:export (modify-features))

; Converts a list of kernel modules into a list of packages.
; Each kernel module should accept the current system kernel
; as a single argument. The kernel module should then dynamically
; create a valid kernel module package based on the specified kernel.
(define-public (kernel-modules->list modules kernel)
               (map (lambda (mod) (mod kernel)) modules))

; Converts a list of package names into the actual package definitions.
(define-public (pkgs lst)
               (map specification->package lst))

; Predicates
(define-public (dotfile? x)
               (and (string? (car x))
                    (or (file-like? (cadr x)) (gexp? (cadr x)))))

(define-public (state-item? x)
               (and (string? (car x)) (string? (cdr x))))

(define-public (list-of-dotfiles? x)
               (every dotfile? x))

(define-public (list-of-state-items? x)
               (every state-item? x))

(define-syntax %modify-feature
  (syntax-rules ()
    ((_ feature (delete kind) clauses ...)
     (if (eq? (feature-name feature) kind)
         #f
         (%modify-feature feature clauses ...)))
    ((_ feature)
     feature)))

(define-syntax modify-features
  (syntax-rules ()
    "Modify the features listed in FEATURES according to CLAUSES and return
    the resulting list of features  Each clause must have the form: (delete FEATURE-NAME)"
    ((_ features clauses ...)
     (filter-map (lambda (feature)
                   (%modify-feature feature clauses ...))
                 features))))
