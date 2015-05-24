# autoloads
autoload -Uz compinit && compinit

# options
setopt histignorealldups sharehistory dvorak correct

# emacs keybindings
bindkey -e

# history settings
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.zshhist

# aliases
alias l='ls -Gh --color'
alias rm='rm -i'
alias ..='cd ..'
alias magit='emacs -f magit-status'
alias grep='egrep --color=auto'

# prompt
PROMPT='%3~ '
