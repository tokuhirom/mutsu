# Bundled zef 1.1.4; LEAVE/KEEP/UNDO in map and grep blocks

`vendor/zef/` was re-vendored from zef 1.1.3 (`0aa54f53`) to 1.1.4
(`a5289115`, 2026-09-17). Upstream changes: the `--precompile` flag is passed
through to the installer backend (`:$precompile` now defaults to `True`), and
an ecosystem index update no longer races other zef processes — each mirror is
fetched into a private staging directory, parsed before it replaces the index,
and written with an atomic rename; `lock-file-protect` keeps its lock file.

The new `Zef::Repository::Ecosystems.update` runs its per-mirror work in a
`.map(-> $uri { UNDO ...; KEEP ...; LEAVE ...; next ... }).head` callback, and
mutsu never ran LEAVE, KEEP or UNDO in a `map`/`grep`/`first` callback that
took the inline fast path: the body was compiled as a top-level chunk, where a
bare scope-exit phaser compiles to nothing (ENTER happened to work). The
callback body is now compiled as the block it is (`do { ... }`) whenever it
carries one of those phasers, so they fire on every exit — normal, `next`,
`last` or an exception — exactly as in a `for` body. Regression test:
`t/control/leave-keep-undo-in-map-block.t`.

Found along the way and filed as #10232: under `mzef`, subs that
`Zef::Repository::Ecosystems` imports from `Zef::Utils::FileSystem` cannot be
resolved at run time, so on 1.1.3 a fresh-`$HOME` index update died with
`Unknown function: lock-file-protect`, and on 1.1.4 the staging directory's
`delete-paths` cleanup fails silently (inside `try`), leaving a copy of the
index behind.
