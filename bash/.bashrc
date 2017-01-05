# use bash-completion
[[ $PS1 && -f /usr/share/bash-completion/bash_completion ]] && \
    . /usr/share/bash-completion/bash_completion

# aliases
alias ls='ls --color=auto'
alias l='ls'
alias ll='ls -alh --color=auto'

export WORKON_HOME=~/.venvs
export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3.5
export EDITOR=/usr/bin/emacsclient
export PATH=$HOME/.cask/bin:$HOME/.local/bin:$PATH
export HISTCONTROL=ignoreboth:erasedups

# prompt
PS1='\e[00;32m[¬º-°]¬\e[39m '

# python virtualenvwrapper
source $HOME/.local/bin/virtualenvwrapper_lazy.sh 
