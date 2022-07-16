#!/bin/sh

# Makes sure that tint2 is always on top of window stack
while :
  do 
    xdotool windowraise $(xwininfo -name tint2 | grep "Window id:" | cut -d " " -f4)
    sleep 1
  done
