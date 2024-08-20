#!/bin/bash

if test "$(gsettings get org.gnome.desktop.interface color-scheme)" = "'default'"; then
    gsettings set org.gnome.desktop.interface color-scheme "prefer-dark"
    gsettings set org.gnome.desktop.interface gtk-theme "adw-gtk3-dark"
    gsettings set org.gnome.desktop.interface icon-theme "Papirus-Dark"
else
    gsettings set org.gnome.desktop.interface color-scheme "default"
    gsettings set org.gnome.desktop.interface gtk-theme "adw-gtk3"
    gsettings set org.gnome.desktop.interface icon-theme "Papirus-Light"
fi
