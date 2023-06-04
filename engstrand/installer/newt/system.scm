(define-module (engstrand installer newt system)
  #:use-module (gnu installer steps)
  #:use-module (gnu installer utils)
  #:use-module (gnu installer newt page)
  #:use-module (gnu installer newt utils)
  #:use-module (guix i18n)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (newt)
  #:use-module (engstrand installer newt menu)
  #:export (run-system-page))

(define* (run-system-page)
  "Run a page that asks the user which existing system config
to use, if any. A new system can also be created if needed."
  (define system-configs
    (map (lambda (config)
           `(,(G_ (basename config ".scm")) . ,(lambda () (display config))))
         (filter (lambda (file) (string-suffix? ".scm" file))
                 (scandir "../../systems"))))

   (G_ "System configuration")
   (G_ "Please select which system configuration file to use for this computer.")
   #:listbox-items
   `(,@system-configs
     (,(G_ "Create new system config")
      .
      ,(lambda () (display "new config"))))
   #:listbox-item->text car)
