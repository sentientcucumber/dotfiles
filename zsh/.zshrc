# autoloads
autoload -Uz compinit colors

compinit && colors

# beautiful 256 colors
[[ $TERM == "xterm" ]] && TERM=xterm-256color

# fuzzy matching
zstyle ":completion:*" completer _complete _match _approximate
zstyle ":completion:*:match:*" original only
zstyle ":completion:*:approximate:*" max-errors 1 numeric

# remove trailing slash
zstyle ":completion:*" squeeze-slashes true

# options
setopt histignorealldups sharehistory dvorak correct emacs
unsetopt beep

# history settings
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.zshistory

# alias land!
alias l="ls -Gh --color"
alias la="ls -Ghla --color"

alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."

alias rm="rm -i"
alias magit="emacs -f magit-status"
alias grep="egrep --color=auto"

alias tmux="tmux -2"

# exports
export PATH="$PATH:$HOME/.npm-packages/bin:$HOME/.cask/bin"

# load files
ZSHD=$HOME/.zsh.d

if [ -d $ZSHD ] ; then
    # load syntax-highlighting
    if [ -e $ZSHD/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ] ; then
        source $ZSHD/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
    fi

    # load remaining files
    setopt EXTENDED_GLOB
    for file in $ZSHD/*.zsh ; do
        source $file
    done
    unsetopt EXTENDED_GLOB
fi
