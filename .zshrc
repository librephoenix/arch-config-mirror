#
#    ______                          _   _     
#   |  ____|                        | | ( )
#   | |__   _ __ ___  _ __ ___   ___| |_|/ ___
#   |  __| | '_ ` _ \| '_ ` _ \ / _ \ __| / __|
#   | |____| | | | | | | | | | |  __/ |_  \__ \
#   |______|_| |_| |_|_| |_| |_|\___|\__| |___/
#                  _
#                 | |
#        ____ ___ | |__   _ __   ___
#       |_  // __|| '_ \ | '__| / __|
#        / / \__ \| | | || |   | (__
#       /___||___/|_| |_||_|    \___|
#

# add doom emacs bin and local bin to PATH on this zshrc
export PATH=$PATH:~/.emacs.d/bin:~/.local/bin

# path to oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# zsh theme
# see https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="agnoster"

# plugins
plugins=(git zsh-autosuggestions zsh-syntax-highlighting)

# source oh-my-zsh
source $ZSH/oh-my-zsh.sh

# custom zsh highlighting colors
ZSH_HIGHLIGHT_STYLES[command]=fg=blue,underline
ZSH_HIGHLIGHT_STYLES[precommand]=fg=blue,underline
ZSH_HIGHLIGHT_STYLES[arg0]=fg=blue

# User configuration
export MANPATH="/usr/local/man:$MANPATH"
export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
else
  export EDITOR='mvim'
fi

# source aliases on this zshrc
source .aliases
