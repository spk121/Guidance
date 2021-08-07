guidance_sources = [
  'main.c',
  'guidance-window.c',
]

guidance_deps = [
  dependency('gio-2.0', version: '>= 2.50'),
  dependency('gtk+-3.0', version: '>= 3.22'),
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
