# Retired mutsu's native `Test::Util` / `Test::Tap` handlers

mutsu used to ship native Rust implementations of roast's `Test::Util` /
`Test::Tap` helpers (`is_run`, `get_out`, `make-temp-file`/`-dir`/`-path`,
`is-eqv`, `group-of`, `doesn't-hang`, `doesn't-warn`, `is-deeply-junction`,
`is-path`, `throws-like-any`) as a rung-3 provider, alongside the real
`roast/packages/Test-Helpers/lib/Test/Util.rakumod`. An earlier slice made an
imported declaration of one of these names beat the native provider
(`user_test_decl_beats_native`), and migrated the handful of `t/*.t` files
that were calling one of them without importing the real module. This slice
does the final step: deleting the now-unreachable native implementations
themselves (`src/runtime/test_functions/subprocess.rs` and `util.rs` in
their entirety, plus the individual functions in `comparison.rs`,
`tap_subtest.rs`, `throws_like.rs`, and `eval_exception.rs`), along with a
few small helpers (`parse_program_with_operators`,
`program_mentions_qx`, `junction_kind_name`/`junction_sort_key`/
`junction_guts_value`) that became dead code once their only caller was
removed.

Deleting the native fallback surfaced four pre-existing bugs in `t/` files
that had been silently relying on it instead of the real, imported
`Test::Util` routine — the native handler answered the call regardless of
whether the file's own import actually worked, so a broken import went
unnoticed:

- `t/cro-client-nested-param-shadow.t` and `t/main-enum-subset.t` used
  `$*PROGRAM.parent(1)` to reach `roast/packages/Test-Helpers`, one level too
  shallow for a file living in `t/`; needed `.parent(2)`.
- `t/compunit-repository-for-name.t` had the same off-by-one.
- `t/when-block-value-not-sunk.t` `use`d `Test::Util` with no `use lib` line
  at all.
- `t/is-deeply-user-raku-diagnostic.t`'s `is-eqv` subtest called the real
  `is-eqv` with only two positional arguments; its signature requires a
  `Str:D` description.

All four are fixed. `t/test-fn-import-shadow.t` gained a regression guard
pinning that a retired name (`make-temp-dir`) now has no native fallback at
all — calling it without importing `Test::Util` is an undeclared routine, not
a silent success.

Split off as its own open finding, unrelated to this deletion:
`todo/tickets/runtime-error-test-failures-printed-to-stderr.md`.
