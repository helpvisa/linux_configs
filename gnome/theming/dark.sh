#!/bin/bash

gsettings set org.gnome.desktop.interface gtk-theme "adw-gtk3-dark"
gsettings set org.gnome.desktop.interface icon-theme "Papirus-Dark"
# for rounded panel corners
gsettings --schemadir ~/.local/share/gnome-shell/extensions/panel-corners@aunetx/schemas set org.gnome.shell.extensions.panel-corners panel-corner-background-color 'rgb(0,0,0)'
