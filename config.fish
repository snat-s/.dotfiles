if status is-interactive
    # Commands to run in interactive sessions can go here
end
set -U fish_greeting
fish_add_path /mnt/sets/progra/m/scripts

# Navigation
alias ..='cd ..'
alias ...='cd ../..'
alias sets='cd /mnt/sets'
alias progra='cd /mnt/sets/progra'
alias m='cd /mnt/sets/progra/m/'
alias c='cd /mnt/sets/progra/c/src'
alias esp='cd /mnt/sets/especialidad/'

# Package management
alias dnfi='sudo dnf install'
alias dnfu='sudo dnf upgrade'

# Editor
alias nv='nvim'

# Python virtual environment
alias acvenv='source venv/bin/activate.fish'
alias uv='uv venv'
alias uvpip='uv pip install'

# Utilities
alias bt='btop'

# Config files
alias nvconf='nvim ~/.config/nvim/init.lua'
alias fconf='nvim ~/.config/fish/config.fish'

# node 
fish_add_path /home/snats/.nvm/versions/node/v22.9.0/bin/
fish_add_path /home/snats/opt/bin/
fish_add_path ~/.local/bin
fish_add_path /mnt/sets/progra/m/scripts/
