project('statusnotifier-systray-gtk4', 'c',
  license         : 'GPL-3.0-only',
  version         : '0.1.0',
  default_options : [
    'c_std=c11',
    'warning_level=2'
  ]
)

add_project_arguments(
  '-D_POSIX_C_SOURCE=200809L',
  '-Wno-unused-parameter',
  '-Wno-missing-field-initializers',
  '-Wdeclaration-after-statement',
  language: 'c'
)

projectname = meson.project_name()

pkg_config = import('pkgconfig')
gnome = import('gnome')

incl = include_directories('src')

if get_option('default_library') == 'static'
  install_doc     = false
  install_gir     = false
else
  install_doc     = true
  install_gir     = true
endif

subdir('src')

if get_option('examples')
  subdir('examples')
endif

if get_option('docs')
  subdir('docs')
endif
