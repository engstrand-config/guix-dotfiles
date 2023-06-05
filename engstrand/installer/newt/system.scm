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
  #:use-module (engstrand installer record)
  #:use-module (engstrand installer newt utils)
  #:export (run-system-page))

(define* (run-system-create-page)
  "Asks the user for information to create a new system config.
A <new-system> record containing the provided information will be returned."
  (define features
    '(("Bluetooth" . bluetooth)
      ("Laptop base features" . laptop)))

  (define name
    (run-input-page (G_ "Please enter the name of the new system.")
                    (G_ "Name")))

  (define feature-selections
    (run-checkbox-tree-page
     #:title (G_ "System features")
     #:info-text (G_ "Select miscellaneous features to use on your system.")
     #:items features
     #:item->text car
     #:checkbox-tree-height 5
     #:exit-button-callback-procedure
     (lambda _
       (abort-to-prompt 'installer-step 'abort))))

  (new-system
   (name name)
   (features (map cdr feature-selections))))

(define* (run-system-page system-configs)
  "Run a page that asks the user which existing system config
to use, if any. A new system can also be created if needed."
  (define selection
    (run-listbox-selection-page
     #:title (G_ "System configuration")
     #:info-text (G_ "Select a system configuration file.")
     #:info-textbox-width 30
     #:listbox-items `(,@system-configs ,%newt-create-new-config-label)
     #:listbox-item->text identity
     #:listbox-height 10
     #:sort-listbox-items? #f
     #:button-text (G_ "Exit")
     #:button-callback-procedure
     (lambda _
       (abort-to-prompt 'installer-step 'abort))))

  (if (eq? selection %newt-create-new-config-label)
      (run-system-create-page)
      selection))
