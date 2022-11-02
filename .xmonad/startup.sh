#!/bin/bash

trayertint=$1

nbColor=$2
nfColor=$3
sbColor=$4
sfColor=$5

# Startup shell script called by xmonad to start necessary programs
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
killall webcord

# pre-launch configurations
#dbus-update-activation-environment --all
xrandr --dpi 120
picom --experimental-backends --daemon &
xset r rate 350 50
setxkbmap -option caps:escape &

# setup necessary environment variables
export QT_QPA_PLATFORMTHEME="qt5ct"

# Launch necessary desktop applications
emacs --daemon &
xautolock -time 25 -locker "xsecurelock & systemctl suspend" &
~/.local/bin/setup_external_monitor.sh &
twmnd &
# nitrogen --restore &
/usr/bin/trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --widthtype request --transparent true --alpha 0 --height 30 --tint $trayertint --monitor "primary" &
#gnome-keyring-daemon --start --components=secrets &
mbsync -a & mu index &
nm-applet &
syncthing --no-browser &
syncthing-gtk -m &
protonmail-bridge --no-window &
flatpak run com.discordapp.Discord --start-minimized &
kdeconnect-indicator &
xmonad --recompile &
xmonad --restart &
#back4.sh 0.04 ~/Media/Backgrounds/steampunk-city.gif &
##sleep 2 && xwinwrap -b -s -fs -st -sp -nf -ov -fdt -- mpv -wid WID --really-quiet --framedrop=vo --no-audio --panscan="1.0" --loop-file=inf --osc=no ~/Downloads/gruvbox-town-mod.gif --scale="bilinear"
