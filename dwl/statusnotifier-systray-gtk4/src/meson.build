sources = files(
  'sndbusmenu.c',
  'snitem.c',
  'snsystray.c',
  'snwatcher.c',
)

headers = files(
  'snsystray.h'
)

deps = [
  dependency('gdk-pixbuf-2.0'),
  dependency('gio-2.0'),
  dependency('glib-2.0'),
  dependency('gobject-2.0'),
  dependency('gtk4', version : '>=4.14'),
]

lib = library(projectname,
              sources,
              version      : meson.project_version(),
              dependencies : deps,
              install      : true)

install_headers(headers)

if get_option('default_library') == 'shared' or get_option('pkgconf_static')
  pkg_config.generate(lib,
                      install_dir : get_option('libdir') / 'pkgconfig')
endif

if get_option('gir')
  statusnotifier_gir = gnome.generate_gir(
    lib,
    dependencies      : deps,
    export_packages   : projectname,
    sources           : sources + headers,
    nsversion         : '0.1',
    namespace         : 'Statusnotifier',
    identifier_prefix : 'Sn',
    includes          : [ 'GObject-2.0', 'Gio-2.0', 'Gtk-4.0' ],
    header            : 'snsystray.h',
    install           : install_gir,
    symbol_prefix     : 'sn'
  )

  if get_option('vala')
    statusnotifier_vapi = gnome.generate_vapi(
      projectname,
      sources: statusnotifier_gir[0],
      packages: 'gtk4',
      install: true
    )
  endif
endif
