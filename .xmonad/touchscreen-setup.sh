#!/bin/sh

# Fix error with touchscreen setup on multiple monitors

# Retrieve xinput id for touchscreen
touchscreenid=$(xinput | grep 'ELAN Touchscreen' | awk '{print $5}' | cut -d '=' -f 2)

# Map touchscreen to the actual screen it is on
xinput map-to-output $touchscreenid eDP1
