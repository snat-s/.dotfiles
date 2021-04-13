#!/bin/bash
#Uninstalling nix:
#1. Restore /etc/bashrc.backup-before-nix back to /etc/bashrc

  sudo mv /etc/bashrc.backup-before-nix /etc/bashrc

#(after this one, you may need to re-open any terminals that were opened while it existed.)

#2. Restore /etc/profile.d/nix.sh.backup-before-nix back to /etc/profile.d/nix.sh

  sudo mv /etc/profile.d/nix.sh.backup-before-nix /etc/profile.d/nix.sh

#(after this one, you may need to re-open any terminals that were opened while it existed.)

#3. Delete the files Nix added to your system:

  sudo rm -rf /etc/nix /nix /root/.nix-profile /root/.nix-defexpr /root/.nix-channels /home/snat-s/.nix-profile /home/snat-s/.nix-defexpr /home/snat-s/.nix-channels

#and that is it.

