#!/bin/sh

emacsclient -e "(org-agenda-list)"
emacsclient -c -e "(cfw:open-org-calendar)"
