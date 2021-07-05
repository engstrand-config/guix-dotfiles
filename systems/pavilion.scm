(define-module (pavilion)
               #:use-module (base-system)
               ;; The general laptop module should be added, name pending
               #:use-module (common-laptop)
               #:use-module (gnu))

(operating-system
  (inherit base-operating-system)
  (host-name "pavilion")
  ;; mute internal speaker
  (kernel-arguments (list "modprobe.blacklist=pcspkr,snd_pcsp"))
  (services (append
              (list
                (set-xorg-configuration
                  (xorg-configuration
                    (extra-config (list %xorg-libinput-config)))))))

  (bootloader
    (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (target "/boot/efi")
      (keyboard-layout keyboard-layout)))
  (mapped-devices (list
                    (mapped-device
                      (source
                        (uuid "19ae0e83-781a-4ad9-b5ac-cfa5d83e2512"))
                      (target "cryptroot")
                      (type luks-device-mapping))))
  (file-systems (cons* (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "1ADC-28E8" 'fat32))
                         (type "vfat"))
                       (file-system
                         (mount-point "/")
                         (device "/dev/mapper/cryptroot")
                         (type "ext4")
                         (dependencies mapped-devices))
                       %base-file-systems)))

(define %xorg-libinput-config
  "
  Section \"InputClass\"
  Identifier \"Touchpads\"
  Driver \"libinput\"
  MatchDevicePath \"/dev/input/event*\"
  MatchIsTouchPad \"on\"
  Option \"DisableWhileTyping\" \"on\"
  Option \"NaturalScrolling\" \"true\"
  EndSection
  Section \"InputClass\"
  Identifier \"Keyboards\"
  Driver \"libinput\"
  MatchDevicePath \"/dev/input/event*\"
  MatchIsKeyboard \"on\"
  EndSection
  ")

