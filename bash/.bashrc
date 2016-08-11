# if not running interactively, don't do anything
[[ $- != *i* ]] && return

# use bash-completion
[[ $PS1 && -f /usr/share/bash-completion/bash_completion ]] && \
    . /usr/share/bash-completion/bash_completion

# aliases
alias l='ls --color=auto'
alias ll='ls -alh --color=auto'

export WORKON_HOME=~/.venvs

# prompt
PS1='[\u@\h \W]\$ '

# default programs
BROWSER=/usr/bin/firefox
EDITOR=/usr/bin/emacsclient
