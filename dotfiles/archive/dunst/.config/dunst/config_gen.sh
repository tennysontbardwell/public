#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"

if [ $(hostname) = "ares" ] ; then
    FONT=12
else
    FONT=12
fi

sed "s/{{ FONT }}/$FONT/g" < $DIR/dunstrc.base > $DIR/dunstrc
