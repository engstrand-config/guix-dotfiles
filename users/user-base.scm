(define-module (users user-base)
    #:use-module (srfi srfi-9)
    #:export (system-user? make-system-user))

(define-record-type <system-user>
    (make-system-user account name email gpg-key github)
    system-user?
    (account system-user-account)
    (name system-user-name)
    (email system-user-email)
    (gpg-key system-user-gpg-key)
    (github system-user-github))
