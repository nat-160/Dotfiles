# PowerShell
New-Item -Path $PROFILE -ItemType SymbolicLink -Value $PWD/powershell/profile.ps1 -Force

# Starship
New-Item -Path $HOME/.config/starship.toml -ItemType SymbolicLink -Value $PWD/starship/starship.toml -Force

# Wezterm
New-Item -Path $HOME/.config/wezterm/wezterm.lua -ItemType SymbolicLink -Value $HOME/Dotfiles/wezterm/wezterm.lua -Force
