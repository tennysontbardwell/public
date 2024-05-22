#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

emacsclient -nw -c "$@"
