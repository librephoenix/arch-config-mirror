#!/bin/bash

trayertint=$1

nbColor=$2
nfColor=$3
sbColor=$4
sfColor=$5

themeGTKName=$6
themeAlacrittyName=$7
themeDoomEmacsName=$8

colorBgNormal=$2
colorBgBright=${27}
colorFgNormal=$3
color01Normal=$9
color01Bright=${10}
color02Normal=${11}
color02Bright=${12}
color03Normal=${13}
color03Bright=${14}
color04Normal=${15}
color04Bright=${16}
color05Normal=${17}
color05Bright=${18}
color06Normal=${19}
color06Bright=${20}
color07Normal=${21}
color07Bright=${22}
color08Normal=${23}
color08Bright=${24}
colorFocus=${25}
colorSecondary=${26}

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
killall discord
killall qjoypad

# pre-launch configurations
#dbus-update-activation-environment --all
xrandr --dpi 120 &
picom --experimental-backends --daemon &
xset r rate 350 50
setxkbmap -option caps:escape &
betterdiscordctl --d-install flatpak reinstall &

# setup necessary environment variables
export QT_QPA_PLATFORMTHEME="qt5ct"
export GTK_THEME=$themeGTKName

sed -i 's/background_color=.*/background_color='$nbcolor'/' ~/.config/twmn/twmn.conf &
sed -i 's/foreground_color=.*/foreground_color='$sbcolor'/' ~/.config/twmn/twmn.conf &

sed -i 's/colors: .*/colors: *'$themeAlacrittyName'/' ~/.config/alacritty/alacritty.yml &
sed -i 's/colors: .*/colors: *'$themeAlacrittyName'/' ~/.config/alacritty/alacritty.org &

sed -i "s/(setq doom-theme .*/(setq doom-theme '"$themeDoomEmacsName")/" ~/.doom.d/config.el &
sed -i "s/(setq doom-theme .*/(setq doom-theme '"$themeDoomEmacsName")/" ~/.doom.d/doom.org &
sed -i "s/(setq doom-theme .*/(setq doom-theme '"$themeDoomEmacsName")/" ~/.doom.d/doom-pub.org &

cp ~/.config/xmobar/base-xmobarrc ~/.config/xmobar/xmobarrc &&
sed -i "s/colorBgNormal/"$colorBgNormal"/g" ~/.config/xmobar/xmobarrc # normal background
sed -i "s/colorBgBright/"$colorBgBright"/g" ~/.config/xmobar/xmobarrc # bright background
sed -i "s/colorFgNormal/"$colorFgNormal"/g" ~/.config/xmobar/xmobarrc # normal foreground
sed -i "s/color01Normal/"$color01Normal"/g" ~/.config/xmobar/xmobarrc # normal black
sed -i "s/color01Bright/"$color01Bright"/g" ~/.config/xmobar/xmobarrc # bright black
sed -i "s/color02Normal/"$color02Normal"/g" ~/.config/xmobar/xmobarrc # normal red
sed -i "s/color02Bright/"$color02Bright"/g" ~/.config/xmobar/xmobarrc # bright red
sed -i "s/color03Normal/"$color03Normal"/g" ~/.config/xmobar/xmobarrc # normal green
sed -i "s/color03Bright/"$color03Bright"/g" ~/.config/xmobar/xmobarrc # bright green
sed -i "s/color04Normal/"$color04Normal"/g" ~/.config/xmobar/xmobarrc # normal yellow
sed -i "s/color04Bright/"$color04Bright"/g" ~/.config/xmobar/xmobarrc # bright yellow
sed -i "s/color05Normal/"$color05Normal"/g" ~/.config/xmobar/xmobarrc # normal blue
sed -i "s/color05Bright/"$color05Bright"/g" ~/.config/xmobar/xmobarrc # bright blue
sed -i "s/color06Normal/"$color06Normal"/g" ~/.config/xmobar/xmobarrc # normal magenta
sed -i "s/color06Bright/"$color06Bright"/g" ~/.config/xmobar/xmobarrc # bright magenta
sed -i "s/color07Normal/"$color07Normal"/g" ~/.config/xmobar/xmobarrc # normal cyan
sed -i "s/color07Bright/"$color07Bright"/g" ~/.config/xmobar/xmobarrc # bright cyan
sed -i "s/color08Normal/"$color08Normal"/g" ~/.config/xmobar/xmobarrc # normal white
sed -i "s/color08Bright/"$color08Bright"/g" ~/.config/xmobar/xmobarrc # bright white
sed -i "s/colorFocus/"$colorFocus"/g" ~/.config/xmobar/xmobarrc # wm focus color
sed -i "s/colorSecondary/"$colorSecondary"/g" ~/.config/xmobar/xmobarrc & # xmobar highlight color

sed -i "s/Nsxiv.window.background: .*/Nsxiv.window.background: "$colorBgNormal"/" ~/.Xresources
sed -i "s/Nsxiv.window.foreground: .*/Nsxiv.window.foreground: "$colorFgNormal"/" ~/.Xresources &

sed -i "s/export GTK_THEME=.*/export GTK_THEME="$themeGTKName"/" ~/.xsession &

# Launch necessary desktop applications
emacs --daemon &
xautolock -time 25 -locker "xsecurelock & systemctl suspend" &
~/.local/bin/setup_external_monitor.sh &
twmnd &
# nitrogen --restore &
/usr/bin/trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --widthtype request --transparent true --alpha 0 --height 30 --tint $trayertint --monitor "primary" &
#gnome-keyring-daemon --start --components=secrets &
qjoypad &
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
