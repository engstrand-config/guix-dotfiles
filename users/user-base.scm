(define-module (users user-base)
               #:use-module (guix records)
               #:export (
                         system-user
                         system-user?
                         system-user-account
                         system-user-name
                         system-user-email
                         system-user-github
                         system-user-gpg-key
                         system-user-sign-commits?))

(define-record-type* <system-user>
                     system-user make-system-user
                     system-user?
                     (account       system-user-account)
                     (name          system-user-name)
                     (email         system-user-email)
                     (github        system-user-github)
                     (gpg-key       system-user-gpg-key
                                    (default ""))
                     (sign-commits? system-user-sign-commits?
                                    (default #f)))
