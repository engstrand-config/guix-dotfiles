;; Custom installer for systems using our dotfiles.
;; It makes it easy to reinstall systems using already existing system
;; definitions, as well as creating new ones.
;;
;; Based on https://github.com/guix-mirror/guix/blob/master/gnu/installer.scm.
(define-module (engstrand installer program)
  #:use-module (guix discovery)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix utils)
  #:use-module (guix ui)
  #:use-module ((guix self) #:select (make-config.scm))
  #:use-module (guix describe)
  #:use-module (guix channels)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (gnu installer utils)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages connman)
  #:use-module (gnu packages cryptsetup)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:autoload   (gnu packages gnupg) (guile-gcrypt)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nano)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xorg)
  #:use-module (gnu system locale)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (web uri)
  #:use-module (engstrand installer record)
  #:export (engstrand-installer-program))

(define module-to-import?
  ;; Return true for modules that should be imported.  For (gnu system …) and
  ;; (gnu packages …) modules, we simply add the whole 'guix' package via
  ;; 'with-extensions' (to avoid having to rebuild it all), which is why these
  ;; modules are excluded here.
  (match-lambda
    (('guix 'config) #f)
    (('gnu 'installer _ ...) #t)
    (('gnu 'build _ ...) #t)
    (('guix 'build _ ...) #t)
    (('guix 'read-print) #t)
    (_ #f)))

(define (get-config-files path)
  (let ((base-path (dirname (current-filename))))
    (map (lambda (config) (G_ (basename config ".scm")))
         (filter (lambda (file) (string-suffix? ".scm" file))
                 (scandir (string-append base-path "/" path))))))

;; Override the default Guix installer steps, such as hostname,
;; timezone, user setup, etc. All of this is already defined in our config files.
(define (engstrand-installer-steps)
  #~(lambda (current-installer)
      (list
       ;; Welcome the user and ask them to choose between manual
       ;; installation and graphical install.
       (installer-step
        (id 'welcome)
        (compute (lambda _
                   ((engstrand-installer-welcome-page current-installer)
                    #$(local-file "aux-files/logo.txt")
                    #:pci-database
                    #$(file-append pciutils "/share/hwdata/pci.ids.gz")))))

       ;; Provide an interface above connmanctl, so that the user can select
       ;; a network susceptible to acces Internet.
       (installer-step
        (id 'network)
        (description (G_ "Network selection"))
        (compute (lambda _
                   ((engstrand-installer-network-page current-installer)))))

       ;; Select which existing system configuration to use, or create a new one.
       (installer-step
        (id 'system)
        (description (G_ "System config selection"))
        (compute (lambda _
                   ((engstrand-installer-system-page current-installer)
                    (list #$@(get-config-files "../systems"))))))

       ;; Select which existing user configuration to use, or create a new one.
       (installer-step
        (id 'user)
        (description (G_ "User config selection"))
        (compute (lambda _
                   ((engstrand-installer-user-page current-installer)
                    (list #$@(get-config-files "../configs"))))))

       ;; Run a partitioning tool allowing the user to modify
       ;; partition tables, partitions and their mount points.
       ;; Do this last so the user has something to boot if any
       ;; of the previous steps didn't go as expected.
       (installer-step
        (id 'partition)
        (description (G_ "Partitioning"))
        (compute (lambda _
                   ((engstrand-installer-partition-page current-installer))))
        (configuration-formatter user-partitions->configuration))

       ;; TODO: What do we show here?
       (installer-step
        (id 'final)
        (description (G_ "Configuration file"))
        (compute
         (lambda (result prev-steps)
           ((engstrand-installer-final-page current-installer)
            result prev-steps)))))))

(define (engstrand-installer-program)
  "Return a file-like object that runs the given INSTALLER."
  (define init-gettext
    ;; Initialize gettext support, so that installer messages can be
    ;; translated.
    #~(begin
        (bindtextdomain "guix" (string-append #$guix "/share/locale"))
        (textdomain "guix")
        (setlocale LC_ALL "")))

  (define set-installer-path
    ;; Add the specified binary to PATH for later use by the installer.
    #~(let* ((inputs
              '#$(list bash ;start subshells
                       connman ;call connmanctl
                       cryptsetup
                       dosfstools ;mkfs.fat
                       e2fsprogs ;mkfs.ext4
                       lvm2-static ;dmsetup
                       btrfs-progs
                       jfsutils ;jfs_mkfs
                       ntfs-3g ;mkfs.ntfs
                       xfsprogs ;mkfs.xfs
                       kbd ;chvt
                       util-linux ;mkwap
                       nano
                       shadow
                       tar ;dump
                       gzip ;dump
                       coreutils)))
        (with-output-to-port (%make-void-port "w")
          (lambda ()
            (set-path-environment-variable "PATH" '("bin" "sbin") inputs)))))

  (define steps (engstrand-installer-steps))

  ;; TODO: Update
  (define modules
    (scheme-modules*
     (string-append (current-source-directory) "/..")
     "gnu/installer"))

  (define installer-builder
    ;; Note: Include GUIX as an extension to get all the (gnu system …), (gnu
    ;; packages …), etc. modules.
    (with-extensions (list guile-gcrypt guile-newt
                           guile-parted guile-bytestructures
                           guile-json-3 guile-git guile-webutils
                           guile-gnutls
                           guile-zlib           ;for (gnu build linux-modules)
                           (current-guix))
      (with-imported-modules `(,@(source-module-closure
                                  `(,@modules
                                    (gnu services herd)
                                    (guix build utils))
                                  #:select? module-to-import?)
                               ((guix config) => ,(make-config.scm)))
        #~(begin
            (use-modules (gnu installer record)
                         (gnu installer keymap)
                         (gnu installer steps)
                         (gnu installer dump)
                         (gnu installer final)
                         (gnu installer hostname)
                         (gnu installer locale)
                         (gnu installer parted)
                         (gnu installer services)
                         (gnu installer timezone)
                         (gnu installer user)
                         (gnu installer utils)
                         (gnu installer newt)
                         ((gnu installer newt keymap)
                          #:select (keyboard-layout->configuration))
                         (gnu services herd)
                         (guix i18n)
                         (guix build utils)
                         (engstrand installer record)
                         (engstrand installer newt)
                         (engstrand installer newt user)
                         (engstrand installer newt utils)
                         (engstrand installer newt system)
                         ((system repl debug)
                          #:select (terminal-width))
                         (ice-9 match)
                         (ice-9 textual-ports))

            ;; Enable core dump generation.
            (setrlimit 'core #f #f)
            (call-with-output-file "/proc/sys/kernel/core_pattern"
              (lambda (port)
                (format port %core-dump)))

            ;; Always use substitutes.
            ((@@ (gnu installer substitutes) enable-discovery))

            ;; Initialize gettext support so that installers can use
            ;; (guix i18n) module.
            #$init-gettext

            ;; Add some binaries used by the installers to PATH.
            #$set-installer-path

            ;; Arrange for language and territory name translations to be
            ;; available.  We need them at run time, not just compile time,
            ;; because some territories have several corresponding languages
            ;; (e.g., "French" is always displayed as "français", but
            ;; "Belgium" could be translated to Dutch, French, or German.)
            (bindtextdomain "iso_639-3"           ;languages
                            #+(file-append iso-codes "/share/locale"))
            (bindtextdomain "iso_3166-1"          ;territories
                            #+(file-append iso-codes "/share/locale"))

            ;; Likewise for XKB keyboard layout names.
            (bindtextdomain "xkeyboard-config"
                            #+(file-append xkeyboard-config "/share/locale"))

            ;; Initialize 'terminal-width' in (system repl debug)
            ;; to a large-enough value to make backtrace more
            ;; verbose.
            (terminal-width 200)

            (define current-installer newt-engstrand-installer)
            (define steps (#$steps current-installer))

            (dynamic-wind
              (engstrand-installer-init current-installer)
              (lambda ()
                (parameterize
                    ((run-command-in-installer
                      (engstrand-installer-run-command current-installer)))
                  (catch #t
                    (lambda ()
                      (define results
                        (run-installer-steps
                         #:rewind-strategy 'menu
                         #:menu-proc (engstrand-installer-menu-page current-installer)
                         #:steps steps))

                      (match (result-step results 'final)
                        ('success
                         ;; We did it!  Let's reboot!
                         (sync)
                         (stop-service 'root))
                        (_
                         ;; The installation failed, exit so that it is
                         ;; restarted by login.
                         #f)))
                    (const #f)
                    (lambda (key . args)
                      (installer-log-line "crashing due to uncaught exception: ~s ~s"
                                          key args)
                      (define dump-dir
                        (prepare-dump key args #:result %current-result))

                      (define user-abort?
                        (match args
                          (((? user-abort-error? obj)) #t)
                          (_ #f)))

                      (define action
                        (if user-abort?
                            'dump
                            ((engstrand-installer-exit-error current-installer)
                             (get-string-all
                              (open-input-file
                               (string-append dump-dir
                                              "/installer-backtrace"))))))
                      (exit 1)))))

              (engstrand-installer-exit current-installer))))))

  (program-file
   "installer"
   #~(begin
       ;; Set the default locale to install unicode support.  For
       ;; some reason, unicode support is not correctly installed
       ;; when calling this in 'installer-builder'.
       (setenv "LANG" "en_US.UTF-8")
       (execl #$(program-file "installer-real" installer-builder
                              #:guile guile-3.0-latest)
              "installer-real"))))
