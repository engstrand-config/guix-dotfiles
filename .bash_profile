# Honor per-interactive-shell startup file
if [ -f ~/.bashrc ]; then . ~/.bashrc; fi

# Export guix configuration
export GUIX_PROFILE="/home/fredrik/.guix-profile"
. "$GUIX_PROFILE/etc/profile"

export PKG_CONFIG_PATH="$GUIX_PROFILE/include"

xset r rate 300 50
killall xcape 2>/dev/null ; xcape -e 'Super_L=Escape'
xmodmap -e 'keycode 135 = Super_R'

xrdb ~/.Xresources
