project('guidance', 'c',
          version: '0.2.0',
    meson_version: '>= 0.50.0',
  default_options: [ 'warning_level=2',
                     'c_std=c2x',
                   ],
)

# Sanitizers are not forced on.  Enable them with meson's built-in
# option, e.g. `meson setup -Db_sanitize=address,undefined _build`.
cc = meson.get_compiler('c')
add_project_arguments(cc.get_supported_arguments([
  '-fstack-protector-strong',
  '-fstack-clash-protection',
  '-fcf-protection',
  '-Werror=shadow',
  '-Werror=incompatible-pointer-types',
  '-Werror=implicit-function-declaration',
  '-fexceptions',
]), language: 'c')

if get_option('analyzer')
  add_project_arguments(cc.get_supported_arguments(['-fanalyzer']),
                        language: 'c')
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
config_inc = include_directories('.')


subdir('data')
subdir('src')
subdir('tests')
subdir('po')

meson.add_install_script('build-aux/meson/postinstall.py')
