# Personal dotfiles
This is my personal dotfiles single repo config using [tuckr](https://github.com/RaphGL/Tuckr)

## Usage

- Clone this repo to `~/.dotfiles` or `~/.config/dotfiles/`
- Install tuckr as per the [repo instructions](https://github.com/RaphGL/Tuckr?tab=readme-ov-file#installation).
- Link the config you want to use:

```shell
tuckr add \* # adds all dotfiles to the system
tuckr add \* -e neovim # adds all dotfiles except neovim
tuckr add neovim zsh # adds the neovim and zsh dotfiles only
tuckr set \* # adds all the dotfiles and runs their hooks (scripts)
tuckr rm \* # removes all dotfiles from your system
```
