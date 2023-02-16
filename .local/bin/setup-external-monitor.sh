#!/bin/sh                                                                                                                                                                                       
# Automatically setup external monitor
#
# From https://sleeplessbeastie.eu/2013/01/07/how-to-automatically-set-up-external-monitor/

xrandr_command="/usr/bin/xrandr"
sed_command="/bin/sed"
is_hdmi_connected=`DISPLAY=:0 $xrandr_command | $sed_command -n '/HDMI-A-0 connected/p'`
 if [ -n "$is_hdmi_connected" ]; then
	  DISPLAY=:0 $xrandr_command --dpi 120 --output HDMI-A-0 --auto --pos 1920x0 --output eDP --auto --pos 1000x1200 --output DisplayPort-1-0 --auto --pos 0x0
	    else
         DISPLAY=:0 $xrandr_command --dpi 120 --output HDMI-A-0 --off --output Display-Port-1-0 --off --output eDP --auto
	fi

nitrogen --restore
xmonad --restart
