#!/bin/sh

xset r rate 300 50
xmodmap ~/.xmodmap
xcape -e 'Super_L=Escape'

# Make sure we have reasonable defaults for font size
[ ! -f ~/.Xresources ] && cp ./.Xdefaults ~/.Xresources

# Load our resources
xrdb -load ~/.Xresources

setbg &
picom &
# dunst &
redshift -r -t 6500:2000 -l 59.8586126:17.6387436 &
$GUIX_PROFILE/libexec/kdeconnectd &

exec dwm
