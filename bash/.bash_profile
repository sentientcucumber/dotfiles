#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/home/geek/.sdkman"
[[ -s "/home/geek/.sdkman/bin/sdkman-init.sh" ]] && source "/home/geek/.sdkman/bin/sdkman-init.sh"
