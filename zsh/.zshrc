# autoloads
autoload -Uz compinit colors
compinit && colors

# fuzzy matching
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 1 numeric

# remove trailing slash
zstyle ':completion:*' squeeze-slashes true

# options
setopt histignorealldups sharehistory dvorak correct emacs
unsetopt beep

# history settings
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.zshhist

# alias land!
alias l='ls -Gh --color'
alias la='ls -Ghla --color'

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'

alias rm='rm -i'
alias magit='emacs -f magit-status'
alias grep='egrep --color=auto'

# prompt
PROMPT="%3~ "

# TODO May be cool to set output as log-esque looking messages
# load files
ZSHD=$HOME/.zsh.d

echo "Loading files from .zsh.d"

if [ -d $ZSHD ] ; then
    # load syntax-highlighting
    if [ -e $ZSHD/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ] ; then
        source $ZSHD/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
        echo "zsh-syntax-highlighting loaded"
    else
        echo "Could not load zsh-syntax-highlighting"
    fi

    
else 
    echo "$ZSHD was not found"
fi
