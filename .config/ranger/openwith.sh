#!/usr/bin/env sh

# first input is FILE
# second input is PROGRAM (callable from terminal)
ranger_scratchpad_keycombo="Super_L+f"

test_result="$(xwininfo -name 'ranger-scratchpad' | grep 'Map State: IsViewable')"
echo $test_result

if [[ "$test_result" == "  Map State: IsViewable" ]]; then
    echo "Ranger scratchpad visible"
    xdotool key $ranger_scratchpad_keycombo && $2 "$1" &
else
    echo "Ranger scratchpad not visible"
    $2 "$1" &
fi
