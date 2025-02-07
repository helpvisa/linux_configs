sources = files(
  'demo.c'
)

deps = [
  dependency('gdk-pixbuf-2.0'),
  dependency('gio-2.0'),
  dependency('glib-2.0'),
  dependency('gobject-2.0'),
  dependency('gtk4'),
]

executable('demo',
  sources,
  include_directories : incl,
  link_with           : [lib],
  dependencies        : [deps],
)
