# nqp::getlexdyn and %*COMPILING<%?OPTIONS>

`nqp::getlexdyn($name)` had no `dispatch_nqp_op` entry at all, so any code
reaching for it died with `Unsupported nqp:: op: nqp::getlexdyn`. This
blocked the `Rakudo::Options` distribution's `blocked_load` record, whose
only exported sub is `nqp::atkey(nqp::getlexdyn('%*COMPILING'),'%?OPTIONS')`.

The fix has two parts, matching the two gaps issue #8572 identified:

- `nqp::getlexdyn` is a genuinely generic dynamic-variable-by-name lookup,
  not a special case for one name. It resolves through
  `Interpreter::get_env_with_main_alias` — the same chokepoint an ordinary
  compiled `%*NAME`/`$*NAME` read already goes through, covering base-tier
  dynamics, lazily-materialized ones (`$*VM`, ...), and user-declared
  dynamics visible by walking the caller chain (`my $*x = ...` in an
  enclosing frame). An unmatched name is a hard error rather than raku's own
  `VMNull` — a low-level ops-only null mutsu has no equivalent of, and one no
  real module exercises (a module only calls `getlexdyn` on a name it knows
  exists).
- `%*COMPILING<%?OPTIONS>` is now a real, seeded dynamic
  (`Interpreter::set_compiling_options`, called once from `main.rs` between
  `Interpreter::new()` and `run()`, same as `set_program_path`/`set_args`).
  It reports mutsu's own subset of what rakudo tracks: `e` (the `-e` source,
  when used), `I` (`-I` paths), `M` (`-M` preloads), and the
  always-present `encoding` (always `utf8` — mutsu has no alternate source
  encodings). `-I`/`-M` collapse to a `Str` for a single value and a `List`
  for repeats, matching rakudo's own single-vs-repeated-flag behavior.

Verified against `raku` directly: `nqp::getlexdyn('%*ENV')` in real rakudo
returns `VMNull` (`%*ENV` lives in `PROCESS::`, not the lexpad dynamic
chain), while `%*COMPILING` and a user-declared dynamic both resolve — the
new `t/vm/nqp-getlexdyn.t` pins mutsu's (intentionally broader) behavior and
documents the divergence rather than silently diverging.

Closes #8572.
