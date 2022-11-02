#!/bin/sh

nbColor=$1
nfColor=$2
sbColor=$3
sfColor=$4

choices=$(/usr/bin/ls ~/.config/libvirt/qemu | grep .xml | cut -f 1 -d '.')

promptarray[0]="What VM?"
promptarray[1]="Which VM?"
promptarray[2]="... What VM do you want?"
promptarray[3]="What VM do you need?"
promptarray[4]="I shall start the VM:"
promptarray[5]="Virtual time?"
promptarray[6]="VM, boss?"
promptarray[7]="Which VM again?"

size=${#promptarray[@]}
index=$(($RANDOM % $size))

selectedprompt=${promptarray[$index]}

choice=$(echo -e "$choices" | dmenu -i -nb ${nbColor} -nf ${nfColor} -sb ${sbColor} -sf ${sfColor} -fn 'UbuntuMono-R:regular:pixelsize=28' -p "$selectedprompt") && exec virt-manager -c qemu:///session --show-domain-console $choice
