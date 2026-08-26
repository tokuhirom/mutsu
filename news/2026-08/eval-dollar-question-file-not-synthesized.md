# `EVAL` now names its own compilation unit, so `$?FILE` and `:filename` work

`EVAL` compiled its argument without giving the resulting compilation unit a
name of its own. `$?FILE` inside the snippet therefore fell through to the
*enclosing* unit's name, and the documented `:filename` named argument was
ignored outright — both `EVAL 'say $?FILE'` and
`EVAL 'say $?FILE', filename => '/my-eval-code'` printed the outer script's
path (or `-e`).

## Root cause

`$?FILE` is folded at parse time from the parser's `SOURCE_FILE` thread-local,
which names the compilation unit currently being parsed
(`src/parser/stmt/simple/lib_paths.rs`). `run.rs` sets it for the mainline and
`load_module` swaps it for each module, but the re-entrant EVAL parse never set
it at all — the comment on `SOURCE_FILE` explicitly said "`None` leaves `$?FILE`
as a runtime lookup (EVAL and other synthesized parses)". That runtime lookup
reads the env key `?FILE`, which still held the enclosing unit's name, so the
snippet silently inherited it. Nothing anywhere consumed a `filename` argument.

## What rakudo actually does (measured against v2026.06)

Each `EVAL` is its own compilation unit with a name, and the name has two
renderings:

- The unit's name is the `:filename` argument if given, otherwise a synthesized
  `EVAL_<N>` where `N` is a per-process counter.
- `Code.file` reports that name **as-is** (`EVAL_0`).
- `$?FILE` reports it **absolutified against `$*CWD`**
  (`/current/dir/EVAL_0`); an already-absolute name such as `/my-eval-code` is
  left alone, and a relative `:filename` is absolutified the same way.

This is the same as-invoked/absolute split the mainline compilation unit already
had (PR #6979). One detail that is easy to get wrong: the counter is consumed
**only when a name is actually synthesized**, so `EVAL $c, :filename` does not
advance it — `EVAL` / `EVAL :filename` / `EVAL` yields `EVAL_0`, the explicit
name, then `EVAL_1`. A nested `EVAL` simply takes the next counter value.

## The fix

`builtin_eval` (`src/runtime/builtins_eval_misc.rs`) now decides the unit name
before compiling: `:filename` when present, otherwise `next_eval_unit_name()`
(an `AtomicUsize` consumed only on synthesis). It installs that name as the env
`?FILE` and its absolutified form as the parser's `SOURCE_FILE` for the duration
of the nested compile-and-run, restoring both afterwards — the same save/restore
shape `run.rs` uses for the mainline. `absolutify_unit_name` resolves against
`$*CWD` rather than the process CWD, so a script that has changed `$*CWD` names
its EVAL units where it thinks it is.

mutsu's output for the ticket's repro is now byte-identical to rakudo's,
including the nested-EVAL counter behavior and the `Code.file` / `$?FILE` split.

Pin: `t/eval-compunit-introspection.t` (also passes verbatim under `raku`).
