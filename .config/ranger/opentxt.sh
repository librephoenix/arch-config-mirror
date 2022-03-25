#!/bin/sh

xdotool key "Super_L+f" && emacsclient "$1" &
