#!/bin/bash
touch $HOME/.config/i3/config.$(hostname)-host
cat $HOME/.config/i3/config.base \
    $HOME/.config/i3/config.$(hostname)-host > $HOME/.config/i3/config
