# The bundled batteries are scanned on first use

Two pieces of every start-up set up the module repositories for programs
that never load a module:

- `ModuleState::new` scanned the bundle directory for its distributions: a
  `read_dir` of ~40 entries and a `stat` of each one's `lib/`. The list is now
  taken on its first use (`Interpreter::bundled_lib_paths`), still from the
  same process-wide memo.
- `add_default_site_repo` decides whether `$*REPO`'s chain gets the `core`
  Installation by asking whether the bundled Rakudo-Core distribution exists.
  It asked by building that distribution's whole value (reading and parsing
  its `META6.json`, building its file hash) and dropping it. It now stats the
  distribution's `lib/` and `META6.json`.

The chain tail is reserved for the lazily linked batteries whether or not the
bundle has any; the first read of it finds out. Refs #19197.
