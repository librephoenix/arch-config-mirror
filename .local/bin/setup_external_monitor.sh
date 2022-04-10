#!/bin/sh                                                                                                                                                                                       
# Automatically setup external monitor
#
# From https://sleeplessbeastie.eu/2013/01/07/how-to-automatically-set-up-external-monitor/

xrandr_command="/usr/bin/xrandr"
sed_command="/bin/sed"

is_hdmi_connected=`DISPLAY=:0 $xrandr_command | $sed_command -n '/HDMI1 connected/p'`

if [ -n "$is_hdmi_connected" ]; then
	  DISPLAY=:0 $xrandr_command --output HDMI1 --auto --right-of eDP1 
  else
	    DISPLAY=:0 $xrandr_command --output HDMI1 --off
fi
nitrogen --restore
