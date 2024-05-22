#!/bin/bash
touch $HOME/.config/xinit/.Xdefaults.$(hostname)-host
cat $HOME/.config/xinit/.Xdefaults.base \
    $HOME/.config/xinit/.Xdefaults.$(hostname)-host > $HOME/.Xdefaults
