#!/bin/sh                                                                                                                                                                                       
# Automatically setup external monitor
#
# From https://sleeplessbeastie.eu/2013/01/07/how-to-automatically-set-up-external-monitor/

xrandr_command="/usr/bin/xrandr"
sed_command="/bin/sed"

# For my laptop
if [ $(hostname) = "ordinal" ]; then
	is_hdmi_connected=`DISPLAY=:0 $xrandr_command | $sed_command -n '/HDMI-A-0 connected/p'`

	if [ -n "$is_hdmi_connected" ]; then
		  DISPLAY=:0 $xrandr_command --output HDMI-A-0 --auto --right-of eDP
		    else
                  DISPLAY=:0 $xrandr_command --output HDMI-A-0 --off
	fi
fi

nitrogen --restore
