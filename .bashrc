#
#    ______                          _   _     
#   |  ____|                        | | ( )
#   | |__   _ __ ___  _ __ ___   ___| |_|/ ___
#   |  __| | '_ ` _ \| '_ ` _ \ / _ \ __| / __|
#   | |____| | | | | | | | | | |  __/ |_  \__ \
#   |______|_| |_| |_|_| |_| |_|\___|\__| |___/
#   _                  _
#  | |                | |
#  | |__    __ _  ___ | |__   _ __   ___
#  | '_ \  / _` |/ __|| '_ \ | '__| / __|
#  | |_) || (_| |\__ \| | | || |   | (__
#  |_.__/  \__,_||___/|_| |_||_|    \___|
#

# add doom emacs bin and local bin to PATH on this bashrc
export PATH=$PATH:~/.emacs.d/bin:~/.local/bin

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Configure default prompt
PS1='[\u@\h \W]\$ '

# Set up hledger
export LEDGER_FILE=~/Family.s/Documents/Finances/2022.journal

# source aliases on this bashrc
source ~/.aliases
