# Paths
#

# Doom emacs bin
export PATH=$PATH:"~/.config/emacs/bin"

# Go
export PATH=$PATH:/usr/local/go/bin
export PATH="$PATH:$(go env GOBIN):$(go env GOPATH)/bin"

# opencode
export PATH=/home/christiaan/.opencode/bin:$PATH

# Pi
export PATH="$HOME/.local/bin:$PATH"

# ASDF
export PATH="${ASDF_DATA_DIR:-$HOME/.asdf}/shims:$PATH"
export PATH="~/.bin:$PATH"
