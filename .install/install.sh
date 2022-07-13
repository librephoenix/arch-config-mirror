#!/bin/sh

# emmet's Arch Config Installation Script

archpackages=()
aurpackages=()

    # browsers
    archpackages+=(
    qutebrowser
    luakit
    )

    # documents
    archpackages+=(
    libreoffice-still
    atril
    xournalpp
    )

    # file managers
    archpackages+=(
    ranger
    pcmanfm
    )

    # media
    archpackages+=(

        # image viewer(s)

        # image editor(s)
        gimp
        krita

        # media players
        vlc
        mpv

        # 3d modelling and video editing
        blender

        # media recording
        cheese
        obs

        # digital audio workstation
        lmms
    )

    # shells
    archpackages+=(
    zsh
    ##fish
    )

    # terminal emulators
    archpackages+=(
    alacritty
    )

    # command line utilities
    archpackages+=(
    grep sed wget which
    bat exa fd bottom ripgrep
    pacman-contrib
    rsync
    systeroid
    tmux
    htop
    hwinfo
    unzip
    )

    # editors
    archpackages+=(
    emacs
    vim
    )

    # development packages

        # android
        archpackages+=(
        android-tools
        android-udev
        )

        # cc
        archpackages+=(
        gcc
        clang
        make
        autoconf
        automake
        libtool
        )

        # python
        archpackages+=(
        python
        python-pip
        )

        # gamedev
        archpackages+=(
        godot
        )

    #fonts
    archpackages+=(
    ttf-font-awesome
    ttf-inconsolata
    ttf-nerd-fonts-symbols-mono
    ttf-ubuntu-font-family
    )

    # virtual machines
    archpackages+=(
    libvirt
    virt-manager
    qemu-full
    lxc
    )

    # core system packages
    archpackages+=(
    linux linux-firmware linux-headers
    base base-devel
    binutils
    git
    fakeroot
    dialog
    xdg-utils
    )

    # security
    archpackages+=(
    xsecurelock xautolock
    ufw gufw
    yubikey-manager
    libsecret gnome-keyring seahorse
    keepassxc
    )

    # graphical display and X utils
    archpackages+=(
    xorg
    autorandr
    xorg-xinit
    xf86-video-vesa
    xf86-video-intel
    ##xf86-video-amd
    xdotool
    xclip
    ddcutil
    )

    # networking
    archpackages+=(
    networkmanager
    network-manager-applet
    wireless_tools
    wpa_supplicant
    dhclient
    dnsmasq
    )

    # file systems
    archpackages+=(
    dosfstools
    )

    # microcode
    archpackages+=(
    intel-ucode
    ##amd-ucode
    )
