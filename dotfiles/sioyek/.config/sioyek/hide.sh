#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

touch ~/Desktop/alert
osascript -e 'tell application "Finder"' -e 'set visible of process "sioyek" to false' -e 'end tell'
