# Honor per-interactive-shell startup file
if [ -f ~/.bashrc ]; then . ~/.bashrc; fi

# Export guix configuration
export GUIX_PROFILE="/home/$USER/.guix-profile"
. "$GUIX_PROFILE/etc/profile"
