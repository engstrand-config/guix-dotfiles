export PATH="$PATH:$HOME/.node_modules/bin:$HOME/.cargo/bin:$HOME/.local/bin/tools:$(du "$HOME/.local/bin/" | cut -f2 | tr '\n' ':' | sed 's/:*$//')"

export EDITOR="nvim"
export TERMINAL="st"
export BROWSER="firefox"
export READER="zathura"
export FILE="vifm"
export STATUSBAR="dsblocks"

export npm_config_prefix=~/.node_modules
export SUDO_ASKPASS="$HOME/.local/bin/tools/dmenupass"
export FZF_DEFAULT_OPTS="-m --height 25% --layout=reverse --color fg:7,bg:-1,hl:1,fg+:232,bg+:6,hl+:255,info:7,prompt:2,spinner:1,pointer:232,marker:1,border:3"

# Export guix configuration
export GUIX_PROFILE="/home/$USER/.guix-profile"
. "$GUIX_PROFILE/etc/profile"

# Source the nix profile and fonts.
if [ -f /run/current-system/profile/etc/profile.d/nix.sh ]; then
    source /run/current-system/profile/etc/profile.d/nix.sh
    export FONTCONFIG_PATH="$(guix build fontconfig)/etc/fonts"
fi

