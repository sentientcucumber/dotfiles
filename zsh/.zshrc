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
alias R='R --no-save'

# history
HISTFILE=$HOME/.zsh-history
HISTSIZE=10000
SAVEHIST=10000

# colors
function colorSetup {
    # ls colors
    autoload colors; colors;
    # A script to make using 256 colors in zsh less painful.
    # P.C. Shyamshankar <sykora@lucentbeing.com>
    # Copied from http://github.com/sykora/etc/blob/master/zsh/functions/spectrum/
    
    typeset -Ag FX FG BG
    FX=(
        reset "%{[00m%}"
        bold "%{[01m%}" no-bold "%{[22m%}"
        italic "%{[03m%}" no-italic "%{[23m%}"
        underline "%{[04m%}" no-underline "%{[24m%}"
        blink "%{[05m%}" no-blink "%{[25m%}"
        reverse "%{[07m%}" no-reverse "%{[27m%}"
    )
    for color in {000..255}; do
        FG[$color]="%{[38;5;${color}m%}"
        BG[$color]="%{[48;5;${color}m%}"
    done
    # Show all 256 colors with color number
    function spectrum_ls() {
        for code in {000..255}; do
            print -P -- "$code: %F{$code}Test%f"
        done
    }
    # Show all 256 colors where the background is set to specific color
    function spectrum_bls() {
        for code in {000..255}; do
            ((cc = code + 1))
            print -P -- "$BG[$code]$code: Test %{$reset_color%}"
        done
    }
}

colorSetup

function precmd {
    # Compute the lengths of the strings
    local pwdsize=${#${(%):-%~}}

    # Global width
    local TERMWIDTH
    (( TERMWIDTH = ${COLUMNS} - 1 ))

    # The horizontal bar
    PR_FILLBAR=""

    # The path, truncated if too long
    PR_PWDLEN=""

    # Compute the path length and the horizontal bar
    if [[ "$pwdsize + 3" -gt $TERMWIDTH ]]; then
        # ((PR_PWDLEN=$TERMWIDTH - $promptsize - $exitcodesize))
        ((PR_PWDLEN=$TERMWIDTH - $pwdsize ))
    else
        PR_FILLBAR="\${(l.(($TERMWIDTH - $pwdsize - 3))..∙.)}"
    fi
}

PROMPT='%$PR_PWDLEN<...<$FG[039][$FG[026]%~$FG[039]]%<< ${(e)PR_FILLBAR}
%{$reset_color%}'
