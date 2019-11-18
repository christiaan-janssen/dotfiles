#!/bin/bash

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
#while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch Polybar, using default config location ~/.config/polybar/config
#polybar example &


for m in $(polybar --list-monitors | cut -d":" -f1); do
  if [ $m = "eDP-1" ]; then
    MONITOR=$m polybar  main &
  fi
  if [ $m = "DP-2-2" ]; then
    MONITOR=$m polybar center &
  fi
  if [ $m = "DP-2-1" ]; then
    MONITOR=$m polybar left &
  fi
done

