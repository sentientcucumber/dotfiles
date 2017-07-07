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

NPM_PACKAGES="${HOME}/.npm-packages"
PATH="$NPM_PACKAGES/bin:$PATH"

unset MANPATH
export MANPATH="$NPM_PACKAGES/share/man:$(manpath)"
