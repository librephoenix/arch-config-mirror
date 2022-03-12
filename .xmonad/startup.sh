#!/bin/bash

# Startup script called by xmonad to start necessary programs
#
## Kill previous instances of applications (Prevents multiple instances of the following if XMonad is restarted durin the X session)
killall xmobar
killall twmnd
killall trayer
killall nm-applet
killall nextcloud
killall xwinwrap
killall gnome-keyring-daemon
killall nitrogen
killall xautolock
killall autokey-gtk

# Launch necessary desktop applications
xautolock -time 5 -locker "xsecurelock" &
gnome-keyring-daemon &
twmnd &
mbsync -a && mu index &
nitrogen --restore
/usr/bin/trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --widthtype request --transparent true --alpha 0 --height 30 --tint 0x28282828 --monitor 0 &
nm-applet &
nextcloud &
protonmail-bridge --no-window &
autokey-gtk &
xmonad --recompile
xmonad --restart
emacs --daemon
##sleep 2 && xwinwrap -b -s -fs -st -sp -nf -ov -fdt -- mpv -wid WID --really-quiet --framedrop=vo --no-audio --panscan="1.0" --loop-file=inf --osc=no ~/Downloads/gruvbox-town-mod.gif --scale="bilinear"
~/.xmonad/workspace-select.sh
