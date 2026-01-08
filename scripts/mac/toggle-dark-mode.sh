#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

osascript -e 'tell app "System Events" to tell appearance preferences to set dark mode to not dark mode'
