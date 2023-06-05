(define-module (engstrand installer record)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:export (<engstrand-installer>
            engstrand-installer
            make-engstrand-installer
            engstrand-installer?
            engstrand-installer-name
            engstrand-installer-init
            engstrand-installer-exit
            engstrand-installer-exit-error
            engstrand-installer-final-page
            engstrand-installer-menu-page
            engstrand-installer-network-page
            engstrand-installer-system-page
            engstrand-installer-user-page
            engstrand-installer-partition-page
            engstrand-installer-welcome-page
            engstrand-installer-run-command

            <new-user>
            new-user
            make-new-user
            new-user?
            new-user-name
            new-user-full-name
            new-user-email

            <new-system>
            new-system
            make-new-system
            new-system?
            new-system-name
            new-system-features))

;;;
;;; Installer record.
;;;

;; The <installer> record contains pages that will be run to prompt the user
;; for the system configuration. The goal of the installer is to produce a
;; complete <operating-system> record and install it.

(define-record-type* <engstrand-installer>
  engstrand-installer make-engstrand-installer
  engstrand-installer?
  ;; symbol
  (name engstrand-installer-name)
  ;; procedure: void -> void
  (init engstrand-installer-init)
  ;; procedure: void -> void
  (exit engstrand-installer-exit)
  ;; procedure (key arguments) -> (action)
  (exit-error engstrand-installer-exit-error)
  ;; procedure void -> void
  (final-page engstrand-installer-final-page)
  ;; procedure: (steps) -> step-id
  (menu-page engstrand-installer-menu-page)
  ;; procedure void -> void
  (network-page engstrand-installer-network-page)
  ;; procedure void -> void
  (system-page engstrand-installer-system-page)
  ;; procedure void -> void
  (user-page engstrand-installer-user-page)
  ;; procedure void -> void
  (partition-page engstrand-installer-partition-page)
  ;; procedure (logo #:pci-database) -> void
  (welcome-page engstrand-installer-welcome-page)
  ;; procedure command -> bool
  (run-command engstrand-installer-run-command))

;;;
;;; New user record.
;;;

;; The <new-user> record contains information about a new user.
;; Information stored in this record will be used when creating a new
;; user configuration.

(define-record-type* <new-user>
  new-user make-new-user
  new-user?
  (name new-user-name)
  (full-name new-user-full-name)
  (email new-user-email))

;;;
;;; New system record.
;;;

;; The <new-system> record contains information about a new system.
;; Information stored in this record will be used when creating a new
;; system configuration.

(define-record-type* <new-system>
  new-system make-new-system
  new-system?
  (name new-system-name)
  (features new-system-features))
