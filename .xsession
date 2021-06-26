#!/bin/sh

xset r rate 300 50
xmodmap ~/.xmodmap
xcape -e 'Super_L=Escape'
xrdb ~/.Xresources

picom &

exec dwm
