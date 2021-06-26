# Bash initialization for interactive non-login shells and
# for remote shells (info "(bash) Bash Startup Files").

# Export 'SHELL' to child processes.  Programs such as 'screen'
# honor it and otherwise use /bin/sh.
export SHELL

# Source the system-wide file.
source /etc/bashrc

export EDITOR="nvim"
export TERMINAL="st"
export BROWSER="firefox"
export READER="zathura"
export FILE="vifm"
export STATUSBAR="dsblocks"

export PASTEL_COLOR_MODE=8bit
export npm_config_prefix=~/.node_modules
export SUDO_ASKPASS="$HOME/.local/bin/tools/dmenupass"
export PATH="$PATH:$HOME/.node_modules/bin:$HOME/.cargo/bin:$HOME/.local/bin/tools:$(du "$HOME/.local/bin/" | cut -f2 | tr '\n' ':' | sed 's/:*$//')"
export FZF_DEFAULT_OPTS="-m --height 25% --layout=reverse --color fg:7,bg:-1,hl:1,fg+:232,bg+:6,hl+:255,info:7,prompt:2,spinner:1,pointer:232,marker:1,border:3"
export PS1="\[$(tput bold)\]\[$(tput setaf 1)\][\[$(tput setaf 3)\]\u\[$(tput setaf 2)\]@\[$(tput setaf 4)\]\h \[$(tput setaf 5)\]\W\[$(tput setaf 1)\]]\[$(tput setaf 7)\]\\$ \[$(tput sgr0)\]"

# less/man colors
export LESS=-R
export LESS_TERMCAP_mb="$(printf '%b' '[1;31m')"; a="${a%_}"
export LESS_TERMCAP_md="$(printf '%b' '[1;36m')"; a="${a%_}"
export LESS_TERMCAP_me="$(printf '%b' '[0m')"; a="${a%_}"
export LESS_TERMCAP_so="$(printf '%b' '[01;44;33m')"; a="${a%_}"
export LESS_TERMCAP_se="$(printf '%b' '[0m')"; a="${a%_}"
export LESS_TERMCAP_us="$(printf '%b' '[1;32m')"; a="${a%_}"
export LESS_TERMCAP_ue="$(printf '%b' '[0m')"; a="${a%_}"

if [[ $- != *i* ]]
then
    # We are being invoked from a non-interactive shell.  If this
    # is an SSH session (as in "ssh host command"), source
    # /etc/profile so we get PATH and other essential variables.
    [[ -n "$SSH_CLIENT" ]] && source /etc/profile

    # Don't do anything else.
    return
fi

alias ls='ls -p --color=auto'
alias ll='ls -l'
alias grep='grep --color=auto'
alias vim='nvim'

# Disable ctrl-s and ctrl-q.
stty -ixon

#Allows you to cd into directory merely by typing the directory name.
shopt -s autocd

# Infinite history
HISTSIZE= HISTFILESIZE=

#[ -f "$HOME/.config/shortcutrc" ] && source "$HOME/.config/shortcutrc" # Load shortcut aliases
#[ -f "$HOME/.config/aliasrc" ] && source "$HOME/.config/aliasrc"

PROMPT_COMMAND='echo -ne "\033]0;${PWD}\007"'

