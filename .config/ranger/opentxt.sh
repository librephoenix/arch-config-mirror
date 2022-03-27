#!/bin/sh

xdotool key "Super_L+f" && emacsclient -c "$1" &
