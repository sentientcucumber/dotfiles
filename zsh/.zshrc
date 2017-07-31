setopt append_history
setopt hist_ignore_dups
setopt hist_ignore_space
setopt share_history

if [ -z "$HISTFILE" ]; then
    HISTFILE=$HOME/.zsh_history
fi

HISTSIZE=1000
SAVEHIST=1000

export XDG_CONFIG_HOME="$HOME/.config"

alias l='ls --color=auto'
alias ll='ls -la --color=auto'
alias grep='grep --color=auto'
alias tmux='tmux -2'

bindkey -v
bindkey -M vicmd 'd' vi-backward-char
bindkey -M vicmd 'n' vi-forward-char
bindkey -M vicmd 'k' vi-delete

PROMPT="%d ❱ "
