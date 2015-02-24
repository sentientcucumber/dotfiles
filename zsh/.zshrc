# config
autoload -U compinit && compinit
autoload -U colors && colors

setopt dvorak
setopt correct

HISTFILE=$HOME/.zsh-history
HISTSIZE=1000
SAVEHIST=1000

#path
export PATH=$PATH:~/.bin:/usr/local/go/bin

#term
export TERM=xterm-256color

# aliases
alias l='ls -Gh'
alias emacs='emacs -nw'
alias rm='rm -i'
alias ..='cd ..'
alias magit='emacs -f magit-status'
alias grep='egrep --color=auto'

# prompt
PROMPT='%3~ : ' 
