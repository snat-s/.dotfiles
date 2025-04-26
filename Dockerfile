FROM archlinux:base

# Update the system and install necessary packages
RUN pacman -Syu --noconfirm && \
    pacman -S --noconfirm \
    git \
    base-devel \
    fish \
    vim \
    neovim \
    tmux \
    curl \
    wget \
    sudo \
    which \
    man \
    openssh

RUN useradd -m -G wheel -s /bin/zsh developer && \
    echo "developer ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/developer

WORKDIR /home/developer

USER developer

COPY --chown=developer:developer dotfiles-setup.sh /home/developer/

RUN chmod +x /home/developer/dotfiles-setup.sh
