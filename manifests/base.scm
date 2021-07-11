(define-module (manifests base)
               #:use-module (gnu packages))

(specifications->manifest
  (list
    "neovim"
    "fzf"
    "bat"
    "fontconfig"
    "harfbuzz"
    "kdeconnect" ;; require for dwm?
    "xwallpaper" ;; require for dwm?
    "xorg-server" ;; require for dwm?
    "xinit"
    "curl" ;; system package?
    "xrandr" ;; cli-utils? video? system package?
    "xsetroot"
    "pamixer" ;; audio profile? or system package?
    "xrdb" ;; require for dwm?
    "setxkbmap"
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
