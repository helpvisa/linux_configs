#!/bin/bash

mkisofs -R -J -l -L -allow-multidot -no-iso-translate -o $2 $1
