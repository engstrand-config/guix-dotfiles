(define-module (engstrand installer newt user)
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
  #:export (run-user-page))

(define* (run-user-create-page)
  "Asks the user for information to create a new user config.
A <new-user> record containing the provided information will be returned."
  (define username
    (run-input-page (G_ "Please enter the name of the new user.")
                    (G_ "Username")))

  (define full-name
    (run-input-page (G_ "Please enter the full name of the new user.")
                    (G_ "Full name")))

  (define email
    (run-input-page (G_ "Please enter the email of the new user.")
                    (G_ "Email")))

  (new-user
   (name username)
   (full-name full-name)
   (email email)))

(define* (run-user-page user-configs)
  "Run a page that asks the user which existing user config
to use, if any. A new user can also be created if needed."
  (define selection
    (run-listbox-selection-page
     #:title (G_ "User configuration")
     #:info-text (G_ "Select a user configuration file.")
     #:info-textbox-width 30
     #:listbox-items `(,@user-configs ,%newt-create-new-config-label)
     #:listbox-item->text identity
     #:listbox-height 10
     #:sort-listbox-items? #f
     #:button-text (G_ "Exit")
     #:button-callback-procedure
     (lambda _
       (abort-to-prompt 'installer-step 'abort))))

  (if (eq? selection %newt-create-new-config-label)
      (run-user-create-page)
      selection))
