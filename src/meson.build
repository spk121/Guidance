guidance_sources = [
  'main.c',
  'guidance-application.c',
  'guidance-application-window.c'
]

guidance_deps = [
  dependency('gio-2.0', version: '>= 2.50'),
  dependency('gtk4', version: '>= 4.2.1'),
  dependency('guile-3.0', version: '>= 3.0.7'),
  dependency('glib-2.0', version: '>= 2.68')
]

gnome = import('gnome')

guidance_sources += gnome.compile_resources('guidance-resources',
  'guidance.gresource.xml',
  c_name: 'guidance'
)

executable('guidance', guidance_sources,
  dependencies: guidance_deps,
  install: true,
)
