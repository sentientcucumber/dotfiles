HISTFILE=~/.zhist
HISTSIZE=1000
SAVEHIST=1000
bindkey -v

zstyle :compinstall filename '/home/shellhead/.zshrc'

autoload -Uz compinit
compinit

export PATH=$PATH:$HOME/.gem/ruby/2.3.0/bin
