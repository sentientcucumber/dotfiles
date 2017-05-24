export PATH="/home/geek/.cask/bin:$PATH"
export XDG_CONFIG_HOME="$HOME/.config"

alias l='ls --color=auto'
alias ll='ls -la --color=auto'
alias grep='grep --color=auto'

precmd() {
    print -rP "%~" 
}

export PROMPT='%(?.%F{075}.%F{202})[¬º-°]¬%f '
