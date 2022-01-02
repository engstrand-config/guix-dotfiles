(define-module (engstrand utils bemenu-prompt)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (gnu packages xdisorg)
  #:export (compute-bemenu-prompt))

(define* (serialize-actions actions)
  "Serializes ACTIONS containing option and command key-value pairs
into a string, separated by new line characters."
  (string-join
   (map (lambda (action) (car action)) actions)
   "\n"))

(define* (compute-bemenu-prompt filename prompt actions #:optional package)
  "Compute a Guile script file named FILENAME containing the necessary code
for launching bemenu with PROMPT and ACTIONS."
  (define (get-exec-args pkg prompt)
    (list (file-append pkg "/bin/bemenu") "-i" "-p" prompt))

  ;; Fallback to upstreamed bemenu package if no package is specified.
  (let ((pkg (if package package bemenu)))
    ;; TODO: Use with-imported-modules?
    (program-file
     filename
     #~(begin
         (use-modules (ice-9 popen)
                      (ice-9 rdelim)
                      (ice-9 match)
                      (srfi srfi-1))
         (call-with-values
             (lambda ()
               (pipeline `(#$(get-exec-args pkg prompt))))
           (lambda (from to pids)
             (display #$(serialize-actions actions) to)
             (close-port to)
             (let ((result (read-line from)))
               (close-port from)
               (match-let* (((pid) pids)
                            ((_ . status) (waitpid pid)))
                 (when (zero? (status:exit-val status))
                   (primitive-eval (assoc-ref `#$actions result)))))))))))
