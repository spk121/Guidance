application_id = 'com.lonelycactus.Guidance'

scalable_dir = join_paths('hicolor', 'scalable', 'apps')
install_data(
  join_paths(scalable_dir, ('@0@.svg').format(application_id)),
  install_dir: join_paths(get_option('datadir'), 'icons', scalable_dir)
)

symbolic_dir = join_paths('hicolor', 'symbolic', 'apps')
install_data(
  join_paths(symbolic_dir, ('@0@-symbolic.svg').format(application_id)),
  install_dir: join_paths(get_option('datadir'), 'icons', symbolic_dir)
)

actions_dir = join_paths('hicolor', '22x22', 'actions')
install_data(
  join_paths(actions_dir, 'debug-memory-view.png'),
  join_paths(actions_dir, 'debug-next-instruction.png'),
  join_paths(actions_dir, 'debug-next.png'),
  join_paths(actions_dir, 'debug-parameter.png'),
  join_paths(actions_dir, 'debug-restart.png'),
  join_paths(actions_dir, 'debug-run.png'),
  join_paths(actions_dir, 'debug-run-to.png'),
  join_paths(actions_dir, 'debug-step-instruction.png'),
  join_paths(actions_dir, 'debug-step-out.png'),
  join_paths(actions_dir, 'debug-step.png'),
  join_paths(actions_dir, 'debug-variable.png'),
  join_paths(actions_dir, 'document-open.png'),
  join_paths(actions_dir, 'edit-trash.png'),
  join_paths(actions_dir, 'edit-shred.png'),
  join_paths(actions_dir, 'gear.png'),
  join_paths(actions_dir, 'help.png'),
  join_paths(actions_dir, 'launch.png'),
  install_dir: join_paths(get_option('datadir'), 'icons', actions_dir)
)
