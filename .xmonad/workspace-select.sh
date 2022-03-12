#!/bin/sh

choices="writing\nbrowsing\ncoding\nmusic\nart\n3dmodel\nfinances\nricing\nother"

promptarray[0]="What do you wish to work on?"
promptarray[1]="Let me guess... ricing?"
promptarray[2]="... What do you want?"
promptarray[3]="New goal for today?"
promptarray[4]="Your wish is my command:"
promptarray[5]="Where would you like to go?"
promptarray[6]="Yeas, boss?"
promptarray[7]="Which workspace?"
promptarray[8]="よし！いくぜ！"

size=${#promptarray[@]}
index=$(($RANDOM % $size))

selectedprompt=${promptarray[$index]}

choice=$(echo -e "$choices" | dmenu -i -nf '#282828' -nf '#ebdbb2' -sb '#458588' -sf '#ebdbb2' -fn 'UbuntuMono-R:regular:pixelsize=28' -p "$selectedprompt")

case "$choice" in
	writing) ~/.xmonad/xmonadctl 1 ;;
	browsing) ~/.xmonad/xmonadctl 3 ;;
	coding) ~/.xmonad/xmonadctl 5 ;;
	music) ~/.xmonad/xmonadctl 7 ;;
	art) ~/.xmonad/xmonadctl 9 ;;
	3dmodel) ~/.xmonad/xmonadctl 11 ;;
	finances) ~/.xmonad/xmonadctl 13 ;;
	ricing) ~/.xmonad/xmonadctl 15 ;;
	other) ~/.xmonad/xmonadctl 17 ;;
esac
