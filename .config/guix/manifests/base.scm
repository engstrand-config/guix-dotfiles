(specifications->manifest
  (list
    "tlp" ;; should be required by the laptop module
    "engstrand-dwm" ;; move to dwm.scm?
    "kdeconnect" ;; require for dwm?
    "xwallpaper" ;; require for dwm?
    "xf86-video-fbdev"
    "xf86-input-libinput"
    "xorg-server" ;; require for dwm?
    "xinit"
    "curl" ;; system package?
    "redshift" ;; night light?
    "brightnessctl" ;; should be required by the laptop module
    "engstrand-dsblocks"
    "stow" ;; system package? Will it be needed when we switch to guix home?
    "xstow"
    "xrandr" ;; cli-utils? video? system package?
    "xsetroot"
    "pamixer" ;; audio profile? or system package?
    "xrdb" ;; require for dwm?
    "engstrand-st" ;; system package? Have an X profile with st, dwm etc. and Wayland profile with dwl, some other terminal?
    "harfbuzz"
    "xmodmap" ;; system package?
    "xcape"
    "ncurses"
    "xset"
    "xkeyboard-config" ;; system package?
    "pkg-config"
    "glib-networking"
    "libxinerama"
    "freetype"
    "libxft"
    "libx11"
    "gcc-toolchain@10"
    "make"
    "node"
    "git" ;; system package?
    "openssh"
    ))
