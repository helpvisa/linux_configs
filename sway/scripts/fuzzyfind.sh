#!/bin/bash

cd $HOME
NEWAPP=$(fzf)
nohup mimeopen -n $NEWAPP &>/dev/null &
sleep 0.5
