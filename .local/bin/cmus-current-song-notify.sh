#!/bin/sh
notify-send "$(cmus-remote -Q | grep title | sed s/'tag title'// ) " "$(cmus-remote -Q | grep album | sed s/'tag album '//)"
