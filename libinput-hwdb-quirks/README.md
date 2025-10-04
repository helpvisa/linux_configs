# libinput device quirks
The Nulea M501 trackball is not actually recognized as a trackball.  This means
that trackball-specific features in certain DEs are not enabled by default.
Adding some custom information to `/etc/udev/hwdb.d` (using the
`71-nulea-trackball-usb.hwdb` file here) and creating some custom quirks in
`/etc/libinput` (using `local-overrides.quirks`) allows us to start treating
the mouse like an actual trackball.

Ideally, this stuff ought to get upstreamed!
