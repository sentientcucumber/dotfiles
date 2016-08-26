# if not running interactively, don't do anything
[[ $- != *i* ]] && return

# use bash-completion
[[ $PS1 && -f /usr/share/bash-completion/bash_completion ]] && \
    . /usr/share/bash-completion/bash_completion

# aliases
alias l='ls --color=auto'
alias ll='ls -alh --color=auto'

# environment variables
export WORKON_HOME=~/.venvs
export EDITOR=/usr/bin/emacsclient

# prompt
PS1='[\u@\h \W]\$ '

# python virtualenvwrapper
source /usr/bin/virtualenvwrapper_lazy.sh 
