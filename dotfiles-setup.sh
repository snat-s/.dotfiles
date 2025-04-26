#!/bin/bash

DOTFILES_DIR="$HOME/.dotfiles"

# check and download packages
check_and_install() {
    if ! pacman -Q "$1" &>/dev/null; then
        echo "$1 is not installed. Installing..."
        sudo pacman -S --needed --noconfirm "$1"
    else
        echo "$1 is already installed."
    fi
}

programs=(
    "libx11"
    "libxft"
    "libxinerama"
    "libxrandr"
    "xorg-server"
    "xorg-xinit"
    "kitty"
    "alacritty"
    "git"
    "fish"
    "neovim"
    "tmux"
    "base-devel"
    "zellij"
    "feh"
)

echo "Checking and installing programs"
for lib in "${programs[@]}"; do
    check_and_install "$lib"
done

echo "All required packages have been checked and installed"

# NOTE: I change this for ssh so i can push easily
if [ ! -d "$DOTFILES_DIR" ]; then
    echo "Cloning dotfiles repository..."
    git clone https://github.com/snat-s/.dotfiles.git "$DOTFILES_DIR"
else
    echo "Dotfiles repository already exists. Pulling latest changes..."
    cd "$DOTFILES_DIR" && git pull
fi
wait

build_and_install() {
	local program=$1
	echo "Building and installing $program..."
	cd "$DOTFILES_DIR/$program" || { echo "Directory for $program not found!"; return 1; }
	sudo make clean install
}

echo "making computer suck more"
build_and_install "dwm-6.5"
build_and_install "slstatus-1.0"
build_and_install "dmenu-5.3"
echo "finished making computer sucking"

# Link configuration files
link_config() {
    local source="$1"
    local target="$2"
    if [ -e "$target" ]; then
        if [ ! -L "$target" ]; then
            echo "Backing up existing $target to $target.bak"
            mv "$target" "$target.bak"
        else
            rm "$target"
        fi
    fi
    ln -s "$source" "$target"
    echo "Created symlink $target -> $source"
}

echo "Setting up configuration symlinks..."
mkdir -p ~/.config/fish
link_config "$DOTFILES_DIR/fish/config.fish" "$HOME/.config/fish/config.fish"

mkdir -p ~/.config/nvim
link_config "$DOTFILES_DIR/nvim" "$HOME/.config/nvim"

mkdir -p ~/.config/alacritty
link_config "$DOTFILES_DIR/alacritty/alacritty.toml" "$HOME/.config/alacritty/alacritty.toml"

mkdir -p ~/.w
#wget https://unsplash.com/photos/UiiHVEyxtyA/download?ixid=M3wxMjA3fDB8MXxhbGx8fHx8fHx8fHwxNzQ1Njg3OTQ3fA&amp;force=true&amp;w=2400 -O ~/.w/wallpaper.jpg 

#&& feh --bg-fill ~/.w/wallpaper.jpg
# obviously need a better .xinitrc
DWM_DESKTOP_ENTRY="[Desktop Entry]
Name=DWM
Comment=Dynamic Window Manager
Exec=sh -c "feh --bg-fill ~/.w/wallpaper.jpg & slstatus & exec dwm"
Type=Application
X-LightDM-DesktopName=DWM
DesktopNames=DWM"

# GDM/LightDM
mkdir -p /usr/share/xsessions
echo "$DWM_DESKTOP_ENTRY" | sudo tee /usr/share/xsessions/dwm.desktop > /dev/null

mkdir -p /usr/share/wayland-sessions
echo "$DWM_DESKTOP_ENTRY" | sudo tee /usr/share/wayland-sessions/dwm.desktop > /dev/null

echo "DWM should now appear as a session option in your login manager!"
echo "Log out and select it from the menu (gear/session icon).
