# History
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=$HISTSIZE
HISTDUP=erase

setopt APPEND_HISTORY
setopt SHARE_HISTORY
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_FIND_NO_DUPS

# Shell behavior
setopt AUTOCD
setopt NOBEEP
setopt NUMERIC_GLOB_SORT # sort file10 after file9, not after file1

eval "$(zoxide init zsh)"

# initialise completions with ZSH's compinit
autoload -Uz compinit && compinit

# Enable interactive completion menu selection
zstyle ':completion:*' menu select

# Make completions case-insensitive
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'

# =========================================================
# Fuzzy finder
# =========================================================

# macOS / Homebrew (Apple Silicon)
if [[ -f /opt/homebrew/opt/fzf/shell/key-bindings.zsh ]]; then
  source /opt/homebrew/opt/fzf/shell/key-bindings.zsh
  source /opt/homebrew/opt/fzf/shell/completion.zsh
fi

# macOS / Homebrew (Intel)
if [[ -f /usr/local/opt/fzf/shell/key-bindings.zsh ]]; then
  source /usr/local/opt/fzf/shell/key-bindings.zsh
  source /usr/local/opt/fzf/shell/completion.zsh
fi

# Arch
if [[ -f /usr/share/fzf/key-bindings.zsh ]]; then
  source /usr/share/fzf/key-bindings.zsh
  source /usr/share/fzf/completion.zsh
fi

# Ubuntu
if [[ -f /usr/share/doc/fzf/examples/key-bindings.zsh ]]; then
  source /usr/share/doc/fzf/examples/key-bindings.zsh
  source /usr/share/doc/fzf/examples/completion.zsh
fi

source ~/zsh/fzf.zsh
source ~/zsh/aliases.zsh
source ~/zsh/bindings.zsh
source ~/zsh/plugins.zsh
source ~/zsh/prompt.zsh
source ~/zsh/path.zsh


if [[ -f "/opt/homebrew/bin/brew" ]] then
  # If you're using macOS, you'll want this enabled
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi

if [[ -f "/home/linuxbrew/.linuxbrew/bin/brew" ]] then 
  eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv zsh)"
fi

# append completions to fpath
fpath=(${ASDF_DATA_DIR:-$HOME/.asdf}/completions $fpath)
   

# Completions
source <(kubectl completion zsh)

