(specifications->manifest
  (list
    "neovim"
    "fzf"
    "bat"
    "fontconfig"
    "harfbuzz"
    "tlp" ;; should be required by the laptop module
    "kdeconnect" ;; require for dwm?
    "xwallpaper" ;; require for dwm?
    "xorg-server" ;; require for dwm?
    "xinit"
    "curl" ;; system package?
    "redshift" ;; night light?
    "brightnessctl" ;; should be required by the laptop module
    "stow" ;; system package? Will it be needed when we switch to guix home?
    "xrandr" ;; cli-utils? video? system package?
    "xsetroot"
    "pamixer" ;; audio profile? or system package?
    "xrdb" ;; require for dwm?
    "xmodmap" ;; system package?
    "ncurses"
    "xset"
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
