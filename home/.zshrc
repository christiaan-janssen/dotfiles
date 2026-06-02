if [[ -f "/opt/homebrew/bin/brew" ]] then
  # If you're using macOS, you'll want this enabled
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi

if [[ -f "/home/linuxbrew/.linuxbrew/bin/brew" ]] then 
  eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv zsh)"
fi

export PATH="${ASDF_DATA_DIR:-$HOME/.asdf}/shims:$PATH"
export PATH="~/.bin:$PATH"
   
eval "$(starship init zsh)"

# Set the directory we want to store zinit and plugins
ZINIT_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}/zinit/zinit.git"

# Download Zinit, if it's not there yet
if [ ! -d "$ZINIT_HOME" ]; then
   mkdir -p "$(dirname $ZINIT_HOME)"
   git clone https://github.com/zdharma-continuum/zinit.git "$ZINIT_HOME"
fi

# Source/Load zinit
source "${ZINIT_HOME}/zinit.zsh"

# Add in zsh plugins
zinit light zsh-users/zsh-syntax-highlighting
zinit light zsh-users/zsh-completions
zinit light zsh-users/zsh-autosuggestions

# Setup nvm
source /usr/share/nvm/init-nvm.sh

# Keybindings
bindkey -e
bindkey '^p' history-search-backward
bindkey '^n' history-search-forward
bindkey '^[w' kill-region

# History
HISTSIZE=5000
HISTFILE=~/.zsh_history
SAVEHIST=$HISTSIZE
HISTDUP=erase
setopt appendhistory
setopt sharehistory
setopt hist_ignore_space
setopt hist_ignore_all_dups
setopt hist_save_no_dups
setopt hist_ignore_dups
setopt hist_find_no_dups

eval "$(atuin init zsh)"

# Aliases
alias ls='eza'
alias vim='nvim'
alias c='clear'
alias ll="eza -la --git"
alias cat="bat --theme \"Monokai Extended Bright\" "
alias gs="git status"
alias tfi="terraform init"
alias tfiu="terraform init -upgrade"
alias tfp="terraform plan"
alias csi="chicken-csi"
alias csc="chicken-csc"
alias tree="tree --gitignore"

# Paths
export PATH=$PATH:"~/.config/emacs/bin"
export PATH="$PATH:$(go env GOBIN):$(go env GOPATH)/bin"

# Rabo Aliases
alias azl="aws-azure-login --mode=gui"
#alias awsume=". $(pyenv which awsume)"
alias kdev="awsume default && kubectl config use-context arn:aws:eks:eu-west-1:643335327026:cluster/dev-bancs-eks"

