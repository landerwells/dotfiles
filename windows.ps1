# Define the source paths of your config files
$alacrittyConfigSource = "~\dotfiles\alacritty."
$nvimConfigSource = "~\dotfiles\nvim"

# Define the destination paths
$alacrittyConfigDest = "$env:APPDATA\alacritty"
$nvimConfigDest = "$env:LOCALAPPDATA\nvim"

# Copy the Alacritty config
Copy-Item -Path $alacrittyConfigSource -Destination $alacrittyConfigDest -Force

# Copy the Neovim config
Copy-Item -Path $nvimConfigSource -Destination $nvimConfigDest -Force

Write-Output "Configuration files have been copied successfully."
