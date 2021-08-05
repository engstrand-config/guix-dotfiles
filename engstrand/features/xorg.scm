(define-module (engstrand features xorg))

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

; TODO: Add packages:
;       "chili-sddm-theme" "engstrand-dwm" "engstrand-dmenu" "engstrand-dsblocks" "engstrand-st"

; TODO: Modify desktop services:
;       (modify-services %desktop-services
;                        (delete gdm-service-type))))

; TODO: Add service:
;       (service sddm-service-type
;                (sddm-configuration
;                  (theme "chili")
;                  (display-server "wayland")
;                  (xorg-configuration
;                    (xorg-configuration
;                      (keyboard-layout keyboard-layout)
;                      (extra-config (append (list %xorg-libinput-config) xorg-extra)))))))
