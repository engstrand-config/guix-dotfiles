(define-module (engstrand utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (rde features)
  #:use-module (farg provider)
  #:use-module (farg colorscheme)
  #:use-module (farg home-service)
  #:use-module (rde features fontutils)
  #:use-module (rde features predicates)
  #:export (
            modify-features
            font->string))

;; Converts a list of kernel modules into a list of packages.
;; Each kernel module should accept the current system kernel
;; as a single argument. The kernel module should then dynamically
;; create a valid kernel module package based on the specified kernel.
(define-public (kernel-modules->list modules kernel)
  (map (lambda (mod) (mod kernel)) modules))

;; Converts a list of package names into the actual package definitions.
(define-public (pkgs lst)
  (map specification->package lst))

;; Helper for removing #<unspecified> from a list.
;; This means that we easily can conditionally add services to the list:
;;
;; @example
;; (list
;;   (simple-service ...)
;;   (simple-service ...)
;;   (when add-keybindings? (simple-service ...)))
;; @end example
(define-public (make-service-list . services)
  (filter (lambda (v) (not (unspecified? v))) services))

;; Predicates
(define-public (dotfile? x)
  (and (string? (car x))
       (or (file-like? (cadr x)) (gexp? (cadr x)))))

(define-public (state-item? x)
  (and (string? (car x)) (string? (cdr x))))

(define-public (list-of-dotfiles? x)
  (every dotfile? x))

(define-public (list-of-state-items? x)
  (every state-item? x))

(define-public (list-of-list? x)
  (every list? x))

(define-syntax %modify-feature
  (syntax-rules ()
    ((_ feature (delete kind) clauses ...)
     ;; HACK: Unwrap features that uses the farg provider.
     (let ((feature-struct (if (procedure? feature)
                               (feature (home-farg-configuration) (make-colorscheme-accessor (colorscheme)))
                               feature)))
         (if (eq? (feature-name feature-struct) kind)
             #f
             (%modify-feature feature clauses ...))))
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

;; Serializes an alist with key-value pairs into an ini configuration file.
;; If no value is specified, only the key will be returned.
;;
;; @example
;; (serialize-ini-config
;;  `(("foo" . "bar")
;;    ("key-with-no-value")))
;; @end
;; yields a string containing newlines:
;; foo=bar
;; key-with-no-value
(define-public (alist->ini filename alist)
  (plain-file filename
              (fold-right
               (lambda (entry acc)
                 (let ((key (car entry))
                       (value (cdr entry)))
                   (string-append
                    key
                    (if (null? value)
                        ""
                        (string-append "="
                                       (if (number? value)
                                           (number->string value)
                                           value)))
                    "\n" acc)))
               ""
               alist)))

(define-public (alist->environment-variable var alist)
  (define (add-arg acc key value)
    (string-append acc " --" key
                   (if (not value) "" (string-append " " value))))

  ;; Join arguments into a single string, with each key prefixed
  ;; with "--" and the key and value separated with a space.
  ;; Values that has no value (or #t) will only add the prefixed key.
  ;; If the value is #f, the key will not be included at all.
  (define str
    (fold
     (lambda (arg acc)
       (let ((key (car arg)) (value (cdr arg)))
         (cond
          ((string? value) (add-arg acc key (string-append "'" value "'")))
          ((number? value) (add-arg acc key (number->string value)))
          ((eq? value #t) (add-arg acc key #f))
          (else acc))))
     "" alist))

  ;; Return an alist containing the environment variable name VAR
  ;; and its value as the result of serializing ALIST.
  `((,var . ,str )))

(define-public (make-log-file name)
  (string-append (or (getenv "XDG_LOG_HOME")
                     (getenv "HOME"))
                 "/" name ".log"))

;; Convert a font into a pango or fcft string that can be used in
;; configuration files.
(define* (font->string fmt type config
                       #:key
                       (bold? '())
                       (size '()))
  (define (weight->string weight)
    (let ((str (symbol->string weight)))
      (match fmt
        ('pango (string-capitalize str))
        ('fcft str)
        (_ str))))

  (define (get-weight font)
    (let ((weight (cond
                   ((null? bold?)
                    (if (font-weight font)
                        (font-weight font)
                        'normal))
                   ((eq? bold? #t) 'bold)
                   ((eq? bold? #f) 'normal))))
      (weight->string weight)))

  (let ((font (get-value type config))
        (fmt-string (if (eq? fmt 'pango) "~a ~a ~a" "~a:style=~a:size=~a")))
    (format #f fmt-string
            (font-name font)
            (get-weight font)
            (if (null? size) (font-size font) size))))

(define-public (str-escape str)
  (string-append "\"" str "\""))
