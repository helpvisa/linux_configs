#!/bin/bash

cd $HOME
NEWAPP=$(fzf)
nohup mimeopen $NEWAPP &
sleep 0.5
