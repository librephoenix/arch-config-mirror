#!/bin/bash

trayertint=$1

nbColor=$2
nfColor=$3
sbColor=$4
sfColor=$5

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
killall caffeine
killall syncthing-gtk

# Launch necessary desktop applications
emacs --daemon &
xautolock -time 5 -locker "xsecurelock & systemctl suspend" &
~/.local/bin/setup_external_monitor.sh &
twmnd &
nitrogen --restore &
/usr/bin/trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --widthtype request --transparent true --alpha 0 --height 30 --tint $trayertint --monitor 0 &
~/.xmonad/workspace-select.sh ${nbColor} ${nfColor} ${sbColor} ${sfColor}
gnome-keyring-daemon
mbsync -a && mu index &
nm-applet &
nextcloud &
syncthing --no-browser &
syncthing-gtk &
protonmail-bridge --no-window &
xmonad --recompile &
xmonad --restart &
##sleep 2 && xwinwrap -b -s -fs -st -sp -nf -ov -fdt -- mpv -wid WID --really-quiet --framedrop=vo --no-audio --panscan="1.0" --loop-file=inf --osc=no ~/Downloads/gruvbox-town-mod.gif --scale="bilinear"
