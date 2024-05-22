#!/usr/bin/env sh

# $1 is path
# $2 is text

cmd="$(printf "/Applications/sioyek.app/Contents/MacOS/sioyek %q --page %s --xloc %s --yloc %s" \
    "$1" \
    "$(expr 1 + "$(echo "$3" | cut -d ' ' -f 1)")" \
    "$(echo "$3" | cut -d ' ' -f 2)" \
    "$(echo "$3" | cut -d ' ' -f 3)" \
    )"

text="$(printf '%s --- %s' "$(basename "$1")" "$2")"

res=$(printf "[[shell:%s][%s]]"  \
    "$cmd" \
    "$text")

# Debugging
(printf "%s\n" "$@"; echo "") > ~/Desktop/b.txt
(printf "%q\n" "$@"; echo "") >> ~/Desktop/b.txt
(printf "%s\n" "$cmd"; echo "") >> ~/Desktop/b.txt
(printf "%s\n" "$text"; echo "") >> ~/Desktop/b.txt
(rawurlencode "$cmd"; echo "") >> ~/Desktop/b.txt
echo "$res" >> ~/Desktop/b.txt

echo "$res" | pbcopy
