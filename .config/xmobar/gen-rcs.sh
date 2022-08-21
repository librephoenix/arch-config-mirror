#!/bin/sh

cp base-xmobarrc gruvbox-xmobarrc
sed -i "s/colorBgNormal/#282828/g" gruvbox-xmobarrc # normal background
sed -i "s/colorBgBright/#383838/g" gruvbox-xmobarrc # bright background
sed -i "s/colorFgNormal/#ebdbb2/g" gruvbox-xmobarrc # normal foreground
sed -i "s/color01Normal/#343428/g" gruvbox-xmobarrc # normal black
sed -i "s/color01Bright/#928374/g" gruvbox-xmobarrc # bright black
sed -i "s/color02Normal/#cc241d/g" gruvbox-xmobarrc # normal red
sed -i "s/color02Bright/#fb4934/g" gruvbox-xmobarrc # bright red
sed -i "s/color03Normal/#98971a/g" gruvbox-xmobarrc # normal green
sed -i "s/color03Bright/#b8bb26/g" gruvbox-xmobarrc # bright green
sed -i "s/color04Normal/#d79921/g" gruvbox-xmobarrc # normal yellow
sed -i "s/color04Bright/#fabd2f/g" gruvbox-xmobarrc # bright yellow
sed -i "s/color05Normal/#458588/g" gruvbox-xmobarrc # normal blue
sed -i "s/color05Bright/#83a598/g" gruvbox-xmobarrc # bright blue
sed -i "s/color06Normal/#b16286/g" gruvbox-xmobarrc # normal magenta
sed -i "s/color06Bright/#d3869b/g" gruvbox-xmobarrc # bright magenta
sed -i "s/color07Normal/#689d6a/g" gruvbox-xmobarrc # normal cyan
sed -i "s/color07Bright/#8ec07c/g" gruvbox-xmobarrc # bright cyan
sed -i "s/color08Normal/#a89984/g" gruvbox-xmobarrc # normal white
sed -i "s/color08Bright/#ebdbb2/g" gruvbox-xmobarrc # bright white
sed -i "s/colorFocus/#458588/g" gruvbox-xmobarrc # wm focus color
sed -i "s/colorSecondary/#d79921/g" gruvbox-xmobarrc # xmobar highlight color

cp base-xmobarrc oceanic-next-xmobarrc
sed -i "s/colorBgNormal/#1b2b34/g" oceanic-next-xmobarrc # normal background
sed -i "s/colorBgBright/#2b3b41/g" oceanic-next-xmobarrc # bright background
sed -i "s/colorFgNormal/#d8dee9/g" oceanic-next-xmobarrc # normal foreground
sed -i "s/color01Normal/#29414f/g" oceanic-next-xmobarrc # normal black
sed -i "s/color01Bright/#405860/g" oceanic-next-xmobarrc # bright black
sed -i "s/color02Normal/#ec5f67/g" oceanic-next-xmobarrc # normal red
sed -i "s/color02Bright/#ff3130/g" oceanic-next-xmobarrc # bright red
sed -i "s/color03Normal/#99c794/g" oceanic-next-xmobarrc # normal green
sed -i "s/color03Bright/#66fa56/g" oceanic-next-xmobarrc # bright green
sed -i "s/color04Normal/#fac863/g" oceanic-next-xmobarrc # normal yellow
sed -i "s/color04Bright/#ffca4f/g" oceanic-next-xmobarrc # bright yellow
sed -i "s/color05Normal/#6699cc/g" oceanic-next-xmobarrc # normal blue
sed -i "s/color05Bright/#4477ee/g" oceanic-next-xmobarrc # bright blue
sed -i "s/color06Normal/#c594c5/g" oceanic-next-xmobarrc # normal magenta
sed -i "s/color06Bright/#d864d8/g" oceanic-next-xmobarrc # bright magenta
sed -i "s/color07Normal/#5fb3b3/g" oceanic-next-xmobarrc # normal cyan
sed -i "s/color07Bright/#30d2d0/g" oceanic-next-xmobarrc # bright cyan
sed -i "s/color08Normal/#65737e/g" oceanic-next-xmobarrc # normal white
sed -i "s/color08Bright/#d8dee9/g" oceanic-next-xmobarrc # bright white
sed -i "s/colorFocus/#c594c5/g" oceanic-next-xmobarrc # wm focus color
sed -i "s/colorSecondary/#5fb3b3/g" oceanic-next-xmobarrc # xmobar highlight color

cp base-xmobarrc dracula-xmobarrc
sed -i "s/colorBgNormal/#282828/g" dracula-xmobarrc # normal background
sed -i "s/colorBgBright/#383838/g" dracula-xmobarrc # bright background
sed -i "s/colorFgNormal/#ebdbb2/g" dracula-xmobarrc # normal foreground
sed -i "s/color01Normal/#343428/g" dracula-xmobarrc # normal black
sed -i "s/color01Bright/#928374/g" dracula-xmobarrc # bright black
sed -i "s/color02Normal/#cc241d/g" dracula-xmobarrc # normal red
sed -i "s/color02Bright/#fb4934/g" dracula-xmobarrc # bright red
sed -i "s/color03Normal/#98971a/g" dracula-xmobarrc # normal green
sed -i "s/color03Bright/#b8bb26/g" dracula-xmobarrc # bright green
sed -i "s/color04Normal/#d79921/g" dracula-xmobarrc # normal yellow
sed -i "s/color04Bright/#fabd2f/g" dracula-xmobarrc # bright yellow
sed -i "s/color05Normal/#458588/g" dracula-xmobarrc # normal blue
sed -i "s/color05Bright/#83a598/g" dracula-xmobarrc # bright blue
sed -i "s/color06Normal/#b16286/g" dracula-xmobarrc # normal magenta
sed -i "s/color06Bright/#d3869b/g" dracula-xmobarrc # bright magenta
sed -i "s/color07Normal/#689d6a/g" dracula-xmobarrc # normal cyan
sed -i "s/color07Bright/#8ec07c/g" dracula-xmobarrc # bright cyan
sed -i "s/color08Normal/#a89984/g" dracula-xmobarrc # normal white
sed -i "s/color08Bright/#ebdbb2/g" dracula-xmobarrc # bright white
sed -i "s/colorFocus/#458588/g" dracula-xmobarrc # wm focus color
sed -i "s/colorSecondary/#d79921/g" dracula-xmobarrc # xmobar highlight color
