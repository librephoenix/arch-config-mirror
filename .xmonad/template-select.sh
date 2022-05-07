#!/bin/sh

choices=$(/usr/bin/ls ~/.xmonad/workspace-templates/)

promptarray[0]="What to do?"
promptarray[1]="Which template?"
promptarray[2]="... What do you want?"
promptarray[3]="What template?"
promptarray[4]="Your template is my command:"
promptarray[5]="What would you like to do?"
promptarray[6]="Yeas, boss?"
promptarray[7]="Which template again?"
promptarray[8]="よし！いくぜ！"

size=${#promptarray[@]}
index=$(($RANDOM % $size))

selectedprompt=${promptarray[$index]}

choice=$(echo -e "$choices" | dmenu -i -nf '#282828' -nf '#ebdbb2' -sb '#458588' -sf '#ebdbb2' -fn 'UbuntuMono-R:regular:pixelsize=28' -p "$selectedprompt")

exec ~/.xmonad/workspace-templates/$choice
