guidance_sources = [
  'main.c',
  'guidance-application-window.c',
  'guidance-application.c',
  'guidance-binding-info.c',
  'guidance-environment-view.c',
  'guidance-environment-info.c',
  'guidance-emoji.c',
  'guidance-frame-info.c',
  'guidance-lisp.c',
  'guidance-module-info.c',
  'guidance-source-view.c',
  'guidance-backtrace-view.c',
  'guidance-thread-info.c',
  'guidance-thread-view.c',
  'guidance-module-info.c',
  'guidance-module-view.c',
  'guidance-terminal-view.c',
  'guidance-trap-info.c',
  'guidance-trap-view.c'
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
