autoload -Uz compinit

compinit

setopt HIST_IGNORE_DUPS HIST_IGNORE_SPACE

HISTFILE=~/.zhist
HISTSIZE=1000
SAVEHIST=1000

bindkey -v

alias l='ls --color=auto'
alias ll='ls -lah --color=auto'

export PROMPT='%5~ '
export WORKON_HOME=$HOME/.virtualenvs
export PATH=$PATH:$HOME/.local/bin

source /usr/bin/virtualenvwrapper_lazy.sh
