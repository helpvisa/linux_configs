gidocgen = find_program('gi-docgen')
docs_dir = get_option('datadir') / 'doc'
doc_target_name = 'statusnotifier-doc'

doc_conf = configuration_data()
doc_conf.set('STATUSNOTIFIER_VERSION', meson.project_version())

statusnotifier_toml = configure_file(input: 'statusnotifier.toml.in',
                                     output: 'statusnotifier.toml',
                                     configuration: doc_conf)

custom_target(doc_target_name,
  input            : statusnotifier_gir[0],
  output           : 'Statusnotifier',
  command          : [
    gidocgen,
    'generate',
    '--quiet',
    '--config', statusnotifier_toml,
    '--content-dir=@0@'.format(meson.current_source_dir()),
    '--output-dir=@OUTPUT@',
    '--no-namespace-dir',
    '--add-include-path=@0@'.format(meson.current_build_dir() / '../src'),
    '@INPUT@' ],
  depends          : [ lib, statusnotifier_gir ],
  depend_files     : [ statusnotifier_toml ],
  build_by_default : true,
  install          : install_doc,
  install_dir      : docs_dir
)
