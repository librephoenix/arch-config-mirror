#!/bin/sh

nbColor=$1
nfColor=$2
sbColor=$3
sfColor=$4

choices="writing\nbrowsing\ncoding\nmusic\nart\nvideo\nfinances\nteaching\ngaming"

promptarray[0]="What do you wish to work on?"
promptarray[1]="Let me guess... ricing?"
promptarray[2]="... What do you want?"
promptarray[3]="New goal for today?"
promptarray[4]="Your wish is my command:"
promptarray[5]="Where would you like to go?"
promptarray[6]="Yeas, boss?"
promptarray[7]="Which workspace?"

size=${#promptarray[@]}
index=$(($RANDOM % $size))

selectedprompt=${promptarray[$index]}

choice=$(echo -e "$choices" | dmenu -i -nb ${nbColor} -nf ${nfColor} -sb ${sbColor} -sf ${sfColor} -fn 'UbuntuMono-R:regular:pixelsize=28' -p "$selectedprompt") && case "$choice" in
	writing) ~/.xmonad/xmonadctl 1 ;;
	browsing) ~/.xmonad/xmonadctl 3 ;;
	coding) ~/.xmonad/xmonadctl 5 ;;
	music) ~/.xmonad/xmonadctl 7 ;;
	art) ~/.xmonad/xmonadctl 9 ;;
	video) ~/.xmonad/xmonadctl 11 ;;
	finances) ~/.xmonad/xmonadctl 13 ;;
	teaching) ~/.xmonad/xmonadctl 15 ;;
	gaming) ~/.xmonad/xmonadctl 17 ;;
esac
