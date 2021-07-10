(define-module (users user-base)
    #:use-module (srfi srfi-9)
    #:export (
        system-user?
        make-system-user
        system-user-account
        system-user-name
        system-user-email
        system-user-github
        system-user-gpg-key
        system-user-sign-commits?))

(define-record-type <system-user>
    (make-system-user account name email github gpg-key sign-commits?)
    system-user?
    (account system-user-account)
    (name system-user-name)
    (email system-user-email)
    (github system-user-github)
    (gpg-key system-user-gpg-key)
    (sign-commits? system-user-sign-commits?))
