#!/bin/sh

xset r rate 300 50
xmodmap ~/.xmodmap
xcape -e 'Super_L=Escape'
xrdb ~/.Xresources

setbg &
picom &
redshift -r -t 6500:2000 -l 59.8586126:17.6387436 &

exec dwm
