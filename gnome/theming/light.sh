#!/bin/bash

gsettings set org.gnome.desktop.interface gtk-theme "adw-gtk3"
gsettings set org.gnome.desktop.interface icon-theme "Papirus-Light"
gsettings set org.gnome.desktop.interface color-scheme "prefer-light"
# for rounded panel corners
gsettings --schemadir ~/.local/share/gnome-shell/extensions/panel-corners@aunetx/schemas set org.gnome.shell.extensions.panel-corners panel-corner-background-color 'rgb(245,245,245)'
