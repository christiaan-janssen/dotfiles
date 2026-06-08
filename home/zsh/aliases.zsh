# Aliases
#

alias ls='eza --icons'
alias ll="eza -lh --icons --git"
alias la="eza -lah --icons --git"

alias tree="eza --tree"

compdef eza=ls

alias vim='nvim'
alias c='clear'

alias cat="bat --theme \"Monokai Extended Bright\" "

alias gs="git status"

alias tfi="terraform init"
alias tfiu="terraform init -upgrade"
alias tfp="terraform plan"

alias csi="chicken-csi"
alias csc="chicken-csc"

alias grep="rg --color=auto"
alias diff='diff --color=auto'
alias df='df -h'

# Rabo Aliases
#alias azl="aws-azure-login --mode=gui"
#alias awsume=". $(pyenv which awsume)"
#alias kdev="awsume default && kubectl config use-context arn:aws:eks:eu-west-1:643335327026:cluster/dev-bancs-eks"
