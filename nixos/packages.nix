{ pkgs, ... }:
{
  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    # CLI / system
    wget
    curl
    tmux
    gnupg
    ripgrep
    git
    networkmanagerapplet

    # Shell
    zsh
    zsh-autoenv
    zsh-autosuggestions
    zsh-completions
    zsh-git-prompt
    zsh-history-substring-search
    zsh-syntax-highlighting
    
    # Editors
    vim
    neovim
    emacs

    # Web
    firefox
    chromium

    # SDL
    SDL2
    SDL2_gfx
    SDL2_image
    SDL2_mixer
    SDL2_net
    SDL2_ttf

    # Languages
    sbcl

    # Games
    cataclysm-dda
  ];

}
