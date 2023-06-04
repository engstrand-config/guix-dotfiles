(define-module (engstrand installer newt user)
  #:use-module (gnu installer steps)
  #:use-module (gnu installer utils)
  #:use-module (gnu installer newt page)
  #:use-module (gnu installer newt utils)
  #:use-module (guix i18n)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (newt)
  #:use-module (engstrand installer newt menu)
  #:export (run-user-page))

(define* (run-user-page)
  "Run a page that asks the user which existing user config
to use, if any. A new user can also be created if needed."
  (define user-configs
    (map (lambda (config)
           `(,(G_ (basename config ".scm")) . ,(lambda () (display config))))
         (filter (lambda (file) (string-suffix? ".scm" file))
                 (scandir "../../configs"))))

   (G_ "User configuration")
   (G_ "Please select which user configuration file to use for this computer.")
   #:listbox-items
   `(,@system-configs
     (,(G_ "Create new user config")
      .
      ,(lambda () (display "new config"))))
   #:listbox-item->text car)
