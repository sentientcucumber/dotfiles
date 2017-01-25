# use bash-completion
[[ $PS1 && -f /usr/share/bash-completion/bash_completion ]] && \
    . /usr/share/bash-completion/bash_completion

# aliases
alias ls='ls --color=auto'
alias l='ls'
alias ll='ls -alh --color=auto'
alias python='python3.5'

export WORKON_HOME=~/.venvs
export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3.5
export EDITOR=/usr/bin/emacsclient
export PATH=$HOME/.cask/bin:$HOME/.local/bin:$PATH
export HISTCONTROL=ignoreboth:erasedups

# prompt
PS1='\e[00;32m\w\e[39m '

# python virtualenvwrapper
source $HOME/.local/bin/virtualenvwrapper_lazy.sh 

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/home/geek/.sdkman"
[[ -s "/home/geek/.sdkman/bin/sdkman-init.sh" ]] && source "/home/geek/.sdkman/bin/sdkman-init.sh"
