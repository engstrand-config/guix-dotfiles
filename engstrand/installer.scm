;;; Copyright © 2019 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 David Wilson <david@daviwil.com>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Based on https://github.com/systemcrafters/guix-install.
;;
;; Generate a bootable image (e.g. for USB sticks, etc.) with:
;; $ guix system image -t iso9660 installer.scm

(define-module (engstrand installer)
  #:use-module (gnu) ;; guix-service-type
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu system)
  #:use-module (gnu system install)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mtools)
  #:use-module (gnu packages package-management)
  #:use-module (nongnu packages linux)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (engstrand systems)
  ;; #:use-module (engstrand installer program)
  #:export (installation-os-nonfree))

(define installation-os-nonfree
  (operating-system
   (inherit installation-os)
   (kernel linux)
   (firmware (list linux-firmware))

   ;; Using vim or Emacs without Caps mapped to Escape is absolute hell.
   (keyboard-layout %engstrand-keyboard-layout)

   ;; Add the 'net.ifnames' argument to prevent network interfaces
   ;; from having really long names.  This can cause an issue with
   ;; wpa_supplicant when you try to connect to a wifi network.
   (kernel-arguments '("quiet" "modprobe.blacklist=radeon" "net.ifnames=0"))

   (services
    (append
     (list
      (simple-service
       'channel-file
       etc-service-type
       (list
        ;; Include the channel file so that it can be used during installation
        `("channels.scm" ,(local-file "channels.scm"))
        ;; Include entire repo since it contains everything we need.
        ;; If reinstalling, the previous system defintion can simply
        ;; by updated with the new file system.
        `("guix-dotfiles" ,(local-file "../../guix-dotfiles" #:recursive? #t)))))
     (modify-services
      ;; Use nonguix substitutes.
      (operating-system-user-services installation-os)
      ;; (kmscon-service-type config => (kmscon-configuration
      ;;                                 (inherit config)
      ;;                                 (login-program (engstrand-installer-program))))
      (guix-service-type config => (guix-configuration
                                    (inherit config)
                                    (substitute-urls
                                     (append
                                      (list "https://substitutes.nonguix.org")
                                      %default-substitute-urls))
                                    (authorized-keys
                                     (append
                                      (list (local-file "./files/nonguix-signing-key.pub"))
                                      %default-authorized-guix-keys)))))))

   ;; Add some extra packages useful for the installation process
   (packages
    (append (list git curl stow vim emacs-no-x-toolkit)
            (operating-system-packages installation-os)))))

installation-os-nonfree
