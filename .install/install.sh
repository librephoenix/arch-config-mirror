#!/bin/sh

# emmet's Arch Config Installation Script

# make sure git is installed
sudo pacman -Syu --noconfirm;
sudo pacman -S --needed --noconfirm git;

# checkout my full dotfiles repo
git clone --bare https://gitlab.com/librephoenix/dotfiles.git .dotfiles.git;
git --git-dir=$HOME/.dotfiles.git --work-tree=$HOME checkout;

# intialize package list bash arrays
archpackages=();
aurpackages=();
flatpackages=();

    # hypr
    aurpackages+=(
    hypr-git
    );

    # hyprland
    aurpackages+=(
    hyprland-bin
    waybar-hyprland-git
    );

    # browsers
    archpackages+=(
    qutebrowser
    luakit
    );

    aurpackages+=(
    brave-bin
    librewolf-bin);

    # documents
    archpackages+=(
    libreoffice-still
    atril
    xournalpp
    );

    aurpackages+=(
    autokey-gtk
    );

    # mail
    archpackages+=(
    geary
    );

    aurpackages+=(
    protonmail-bridge-bin
    mu
    mbsync
    );

    # file sync
    archpackages+=(
    syncthing
    );

    aurpackages+=(
    syncthing-gtk-python3
    );

    # file managers
    archpackages+=(
    ranger
    pcmanfm
    );

    # for ranger
    aurpackages+=(
    dragon-drop
    );

    # media
    archpackages+=(

        # image editor(s)
        gimp
        krita

        # media players
        vlc
        mpv
        youtube-dl

        # 3d modelling and video editing
        blender

        # media recording
        cheese
        obs-studio

        # digital audio workstation
        lmms
    );

    aurpackages+=(
        # image viewers
        nsxiv

        # media players
        freetube-bin
        myuzi

        # media recording
        audio-recorder

        # misc
        betterdiscordctl-git
    );

    flatpackages+=(
    com.discordapp.Discord
    );

    # games and relevant apps
    archpackages+=(
    steam
    lutris
    retroarch
    retroarch-assets-ozone
    libretro-desmume
    libretro-genesis-plus-gx
    libretro-mgba
    );

    aurpackages+=(
    airshipper # veloren
    nbtexplorer-bin
    );

    flatpackages+=(
    com.mojang.Minecraft
    );

    # shells
    archpackages+=(
    zsh
    ##fish
    );

    # terminal emulators
    archpackages+=(
    alacritty
    kitty
    );

    # command line utilities
    archpackages+=(
    neofetch lolcat cowsay
    grep sed wget which
    bat exa fd bottom ripgrep
    pacman-contrib
    rsync
    systeroid
    tmux
    htop
    hwinfo
    unzip
    octave
    reflector
    brightnessctl
    w3m
    );

    aurpackages+=(
    mimeo
    pandoc-bin
    );

    # editors
    archpackages+=(
    vim
    );

    aurpackages+=(
    emacs-gcc-wayland-devel-bin
    vim-plug
    );

    # development packages

        # android
        archpackages+=(
        android-tools
        android-udev
        );

        # cc
        archpackages+=(
        gcc
        clang
        make
        cmake
        autoconf
        automake
        libtool
        );

        # python
        archpackages+=(
        python
        python-pip
        );

        # haskell
        aurpackages+=(
        haskell-language-server-static
        );

        # java
        archpackages+=(
        jdk-openjdk
        jre-openjdk
        gradle
        );

        # gamedev
        archpackages+=(
        godot
        );

        # other
        archpackages+=(
        texinfo
        libffi zlib # required to build xmonad + xmobar
        );

    #fonts
    archpackages+=(
    ttf-font-awesome
    ttf-inconsolata
    ttf-nerd-fonts-symbols-mono
    ttf-ubuntu-font-family
    terminus-font
    );

    aurpackages+=(
    otf-inconsolata-powerline-git
    ttf-unifont
    );

    # desktop utils
    archpackages+=(
    sddm
    dmenu
    tint2
    nitrogen
    lxappearance
    mate-icon-theme
    );

    aurpackages+=(
    picom-jonaburg-git
    qt5-styleplugins
    qt5ct
    xwinwrap-git
    sddm-sugar-dark
    );

    # graphical display and X utils
    archpackages+=(
    xorg
    autorandr
    xorg-xinit
    ##xf86-video-vesa
    ##xf86-video-intel
    xf86-video-amdgpu
    xdotool
    xclip
    ddcutil
    );

    aurpackages+=(
    caffeine-ng
    sct
    );

    # wayland utils
    archpackages+=(
    xdg-desktop-portal-wlr
    swayidle
    swaylock
    grim
    slurp
    );

    aurpackages+=(
    wlsunset
    hyprpaper-git
    );

    # pipewire for audio server
    archpackages+=(
    pipewire
    wireplumber
    helvum
    pipewire-alsa
    pipewire-pulse
    pavucontrol
    pipewire-jack
    );

    # virtual machines
    archpackages+=(
    libvirt
    virt-manager
    qemu-full
    lxc
    swtpm
    );

    # core system packages
    archpackages+=(
    linux linux-firmware linux-headers
    base base-devel
    binutils
    git
    git-delta
    fakeroot
    dialog
    xdg-utils
    cups
    gparted
    flatpak
    );

    aurpackages+=(
    auto-cpufreq
    );

    # security
    archpackages+=(
    xsecurelock xautolock
    ufw gufw
    yubikey-manager
    libsecret gnome-keyring seahorse
    keepassxc
    );

    # networking
    archpackages+=(
    networkmanager
    network-manager-applet
    wireless_tools
    wpa_supplicant
    dhclient
    dnsmasq
    );

    aurpackages+=(
    protonvpn
    rdm-bin
    );

    # file systems
    archpackages+=(
    dosfstools
    );

    # microcode
    archpackages+=(
    ##intel-ucode
    amd-ucode
    );

# install arch packages
sudo pacman -S --needed --noconfirm "${archpackages[@]}";

# install paru if it isn't already installed
sudo pacman -S --needed --noconfirm base-devel;
if ! command -v paru &> /dev/null
   then
      cd /tmp;
      git clone https://aur.archlinux.org/paru.git;
      cd paru;
      makepkg -si;
fi;
cd ~;

# install aur packages
paru -S --needed --noconfirm "${aurpackages[@]}";

# install flatpaks
flatpak install "${flatpackages[@]}";

# apply my gtk themes to all flatpaks
sudo flatpak override --filesystem=$HOME/.themes;
sudo flatpak override --env=GTK_THEME=OffcialDracula;
sudo flatpak override --env=QT_STYLE_OVERRIDE=qt5ct --filesystem=~/.config/qt5ct

# setup file uploads with Discord (files are sandboxed into ~/.discord_launchpad; this works with my ranger config)
mkdir ~/.discord_launchpad;
sudo flatpak override com.discordapp.Discord --filesystem=$HOME/.discord_launchpad

# set up betterdiscord
betterdiscordctl -i flatpak install

# install stack
curl -sSL https://get.haskellstack.org/ | sh;

# install xmonad and xmobar

# go to .xmonad working directory
cd ~/.xmonad;

# clone xmonad, xmonad-contrib, and xmobar
git clone https://github.com/xmonad/xmonad ~/.xmonad/xmonad-git;
git clone https://github.com/xmonad/xmonad-contrib ~/.xmonad/xmonad-contrib-git;
git clone https://codeberg.org/xmobar/xmobar.git ~/.xmonad/xmobar-git;

# setup stack and install
stack setup;
stack install;

# compile xmonadctl binary
stack ghc xmonadctl.hs;

# install hledger

stack install hledger;

# install doom
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d; &&
~/.emacs.d/bin/doom install;
~/.emacs.d/bin/doom sync;

# install oh-my-zsh with unattended flag
sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" --unattended; &&

# re-apply my existing config
mv ~/.zshrc.pre-oh-my-zsh ~/.zshrc; &&

# get zsh plugins
git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions; &&

git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting; &&

chsh -s /bin/zsh;

# post install reminders
echo ""
echo "Post Install Reminders"
echo "-------------------"
echo ""
echo "Configure wallpaper via nitrogen"
echo "Set up mbsync and mu4e, including mu-1.16.5"
echo "Transfer relevant files via backups and syncthing"
echo "Configure larger and nicer fonts for the tty"
echo "Set up ssh keys for servers and git"
