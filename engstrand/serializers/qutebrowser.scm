(define-module (engstrand serializers qutebrowser)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (engstrand utils)
  #:export (serialize-qutebrowser-config))

(define (serialize-qutebrowser-value value)
  (cond
   ((symbol? value) (symbol->string value))
   ((number? value) (number->string value))
   ((string? value) (str-escape value))
   ((list? value) (fold
                   (lambda (x acc)
                     (if (eq? acc "")
                         (str-escape x)
                         (string-append acc ", " (str-escape x))))
                   ""
                   value))
   (else value)))

(define (serialize-qutebrowser-bindings bindings)
  (fold
   (lambda (entry acc)
     (let ((str-value (serialize-qutebrowser-value entry)))
       (string-append acc "config.bind(" str-value ")\n")))
   ""
   bindings))

(define (serialize-qutebrowser-properties properties)
  (fold
   (lambda (entry acc)
     (let ((str-value (serialize-qutebrowser-value (cdr entry))))
       (string-append acc "c." (car entry) " = " str-value "\n")))
   ""
   properties))

(define* (serialize-qutebrowser-config properties bindings)
  "Serializes qutebrowser properties and keybindings into a config.py."
  (string-append
   "config.load_autoconfig()\n"
   (serialize-qutebrowser-properties properties)
   (serialize-qutebrowser-bindings bindings)))
