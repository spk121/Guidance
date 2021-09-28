project('guidance', 'c',
          version: '0.1.0',
    meson_version: '>= 0.50.0',
  default_options: [ 'warning_level=1',
                     'c_std=c2x',
                     'b_sanitize=address',
                     'b_sanitize=leak',
                     'b_sanitize=undefined'
                   ],
)

if meson.get_compiler('c').get_id() == 'gcc'
  add_global_arguments([
  '-fstack-protector-strong',
  '-fstack-clash-protection',
  '-fcf-protection',
  '-Werror=shadow',
  '-fexceptions',
  '-fvar-tracking',
  '-fanalyzer'], language : 'c')
endif

i18n = import('i18n')

config_h = configuration_data()
config_h.set_quoted('PACKAGE_VERSION', meson.project_version())
config_h.set_quoted('GETTEXT_PACKAGE', 'guidance')
config_h.set_quoted('LOCALEDIR', join_paths(get_option('prefix'), get_option('localedir')))
configure_file(
  output: 'guidance-config.h',
  configuration: config_h,
)
add_project_arguments([
  '-I' + meson.build_root(),
], language: 'c')


subdir('data')
subdir('src')
subdir('po')

meson.add_install_script('build-aux/meson/postinstall.py')
