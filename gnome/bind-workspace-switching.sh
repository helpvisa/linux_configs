#!/bin/sh

# first we unbind lock and hide window functionality
gsettings set org.gnome.desktop.wm.keybindings minimize "[]"
gsettings set org.gnome.settings-daemon.plugins.media-keys screensaver "[]"

# now setup new keybindings
# for switching
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-right "['<Super>Page_Down', '<Super><Alt>Right', '<Control><Alt>Right', '<Super>l']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-left "['<Super>Page_Up', '<Super><Alt>Left', '<Control><Alt>Left', '<Super>h']"
# and moving windows
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-right "['<Super><Shift>Page_Down', '<Super><Shift><Alt>Right', '<Control><Shift><Alt>Right', '<Super><Shift>l']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-left "['<Super><Shift>Page_Up', '<Super><Shift><Alt>Left', '<Control><Shift><Alt>Left', '<Super><Shift>h']"
