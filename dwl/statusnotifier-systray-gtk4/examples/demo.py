#!/usr/bin/env python

# To run without installing:
# (PWD is the project root, "build" was chosen as the meson build dir name)
# export LD_LIBRARY_PATH="$PWD/build/src"
# export GI_TYPELIB_PATH="$PWD/build/src"

import gi
gi.require_version('Gdk', '4.0')
gi.require_version('Gtk', '4.0')
gi.require_version('Statusnotifier', '0.1')

from gi.repository import Gdk
from gi.repository import Gtk
from gi.repository import Statusnotifier

def on_activate(app):
    display = Gdk.Display.get_default()
    mons = display.get_monitors()
    cssp = Gtk.CssProvider.new()

    cssp.load_from_string("systray{background-color:#222222;}")
    Gtk.StyleContext.add_provider_for_display(display, cssp, Gtk.STYLE_PROVIDER_PRIORITY_USER)

    for i in range(mons.get_n_items()):
        mon = mons.get_item(i)
        window = Gtk.Window()
        tray = Statusnotifier.Systray.new(22, 4, 4, mon.get_connector())

        tray.set_halign(Gtk.Align.CENTER)
        tray.set_valign(Gtk.Align.CENTER)

        window.set_default_size(60, 60)
        window.set_child(tray)
        window.set_application(app)
        window.present()

app = Gtk.Application(application_id='org.example.demo')
app.connect('activate', on_activate)
app.run(None)
