{ config, ... }:
{
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.christiaan = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  };
}
