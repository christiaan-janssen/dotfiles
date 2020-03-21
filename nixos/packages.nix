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
    zsh
    git

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
  ];

}
