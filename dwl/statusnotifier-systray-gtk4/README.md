# statusnotifier-systray-gtk4

Custom widget for GTK4 that represents a system tray.
To be used in GTK based desktop panels and other desktop components.

I created this for myself to have a system tray in the DWL wayland compositor.
Api is limited currently.

## Dependencies

* [GTK4](https://www.gtk.org/)
* __If `gir` enabled:__ [GObject introspection](https://gitlab.gnome.org/GNOME/gobject-introspection/)
* __If `vala` enabled:__ [Vala](https://wiki.gnome.org/Projects/Vala) (requires gir) (build)
* __If `docs` enabled:__ [gi-docgen](https://gitlab.gnome.org/GNOME/gi-docgen) (requires gir) (build)
* `pkg-config` can be used to integrate the library to your project.
* [Meson](https://mesonbuild.com/) (build)
* [Ninja](https://ninja-build.org/) (build)

Bundled gi-docgen is included as a fallback, in case a system-wide installation is not found.

## Building and installing as a shared library
1. `$ meson setup --prefix=/usr/local build`
2. `$ meson compile -C build`
3. `$ sudo meson install -C build`
4. If the chosen install prefix was /usr/local, the runtime linker most likely
   needs to be configured to find the library. Possibilities are:
    - edit /etc/ld.so.conf
    - add `/usr/local/lib` to `LD_LIBRARY_PATH` environment variable.
5. `$ sudo ldconfig`

### Using the shared library

#### C programs
0. /usr/local/* is not in the pkg-config search path on most systems, so
   to help pkg-config to find the library, export variable
   `PKG_CONFIG_PATH=/usr/local/lib/pkgconfig`
1. Import the library to your build system using pkg-config.
2. Include header `snsystray.h`

#### Other languages using gobject-introspection
1. `GI_TYPELIB_PATH=/usr/local/lib/girepository-1.0` environment variable might
   be required if the chosen install prefix was `/usr/local`.

## Example of building and using as a static library

1. `$ meson setup --default-library=static --prefix=/ -Dgir=false -Dvala=false -Ddocs=false build`
2. `$ meson compile -C build`
3. `$ DESTDIR=path-to-your-project-root/thirdparty meson install -C build`
    - This will install `libstatusnotifier-systray-gtk4.a` to your-project-root/thirdparty/lib and
      `snsystray.h` to your-project-root/thirdparty/include.
4. Include header `snsystray.h` in your project.
5. Link against the library. In gcc, `-Ithirdparty/include` in the compiling step,
   and `-Lthirdparty/lib -lstatusnotifier-systray-gtk4` in the linking step.

## Acknowledgements

Parts of the code are borrowed from:
- KDE
    - https://invent.kde.org/frameworks/kstatusnotifieritem
- libdbusmenu
    - https://launchpad.net/libdbusmenu
- Waybar
    - https://github.com/Alexays/Waybar
