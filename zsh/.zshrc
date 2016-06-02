autoload -Uz compinit
compinit

HISTFILE=~/.zhist
HISTSIZE=1000
SAVEHIST=1000
bindkey -v

export PATH=$PATH:$HOME/.gem/ruby/2.3.0/bin

alias l='ls --color=auto'
alias ll='ls -la --color=auto'
