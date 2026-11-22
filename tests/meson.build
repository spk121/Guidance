guile = find_program('guile-3.0', 'guile', required: false)
src_dir = join_paths(meson.current_source_dir(), '..', 'src')
stubs_dir = join_paths(meson.current_source_dir(), 'stubs')

test_env = environment()
test_env.set('LC_ALL', 'C.UTF-8')
test_env.set('GUILE_AUTO_COMPILE', '1')
test_env.set('XDG_CACHE_HOME', join_paths(meson.current_build_dir(), 'cache'))
test_env.set('ASAN_OPTIONS', 'detect_leaks=0')

if guile.found()
  # The debugger core, in plain Guile with the GUI stubbed out.  The
  # debug VM engine is needed for traps, as in Guidance itself.
  test('repl', guile,
    args: ['--debug', '-L', src_dir, '-L', stubs_dir,
           files('repl-test.scm')],
    env: test_env,
    timeout: 120,
  )
endif

test('gui-smoke', find_program('gui-smoke.sh'),
  args: [guidance_exe, join_paths(meson.current_source_dir(), 'programs')],
  env: test_env,
  timeout: 120,
)
