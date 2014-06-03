# autoload
autoload -U compinit && compinit
autoload -U colors && colors
autoload -Uz vcs_info

# path
export PATH=~/bin:/usr/local/bin:/usr/local/mysql/bin:$PATH

# options
setopt AUTO_CD PROMPT_SUBST HISTIGNOREALLDUPS NO_BEEP

# alias
alias l='ls -Gh'
alias ls='ls -Gh'
alias ..='cd ..'
alias emacs='TERM=xterm /Applications/Emacs.app/Contents/MacOS/Emacs -nw'
alias tmux='tmux -2'

# prompt
PROMPT='%3~ '

# history
HISTFILE=$HOME/dotfiles/zsh/.zsh-history
HISTSIZE=10000
SAVEHIST=10000
