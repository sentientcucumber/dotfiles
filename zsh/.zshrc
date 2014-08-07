# autoload
autoload -U compinit && compinit
autoload -U colors && colors
autoload -Uz vcs_info
autoload -U add-zsh-hook

# path
export PATH=~/bin:/usr/local/bin:$PATH

# options
setopt auto_cd
setopt prompt_subst
setopt histignorealldups
setopt no_beep
setopt dvorak
setopt share_history
setopt correct

# alias
alias l='ls -Gh'
alias ls='ls -Gh'
alias ..='cd ..'
alias emacs='TERM=xterm-256color /Applications/Emacs.app/Contents/MacOS/Emacs -nw'
alias tmux='tmux -2'
alias rm='rm -i'

# history
HISTFILE=$HOME/dotfiles/zsh/.zsh-history
HISTSIZE=10000
SAVEHIST=10000

# prompt
local NEWLINE=$'\n'
local red="%{$fg[red]%}"
local green="%{$fg[green]%}"
local reset="%{$reset_color%}"

PROMPT="${green}%~${NEWLINE}${reset}"
