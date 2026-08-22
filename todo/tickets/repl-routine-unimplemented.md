# `repl()` global routine is unimplemented (mutsu already has the REPL machinery to reuse)

Found by the doc-diff harness (`docs/doc-diff-backlog.md`,
`Type/independent-routines.rakudoc:148`).

## Repro

```raku
my $name = "Alice";
say "Hello, $name";
repl();
say "Goodbye, $name"
```

- `raku`: runs, drops into an interactive REPL at `repl()`, then continues after the
  user exits it.
- `mutsu` (`target/debug/mutsu`): fails to compile —
  `===SORRY!=== Error while compiling ...\nUndeclared routine:\n    repl used`.

## Root cause

`repl` is documented as an independent routine
(`raku-doc/doc/Type/independent-routines.rakudoc:134`, `sub repl()`) but is not
registered as a builtin sub in mutsu at all.

mutsu already has a REPL implementation used when the interpreter is invoked with no
arguments (`mutsu::repl::run_repl()`, wired up in `src/main.rs:326`). Implementing the
`repl()` builtin should mostly be a matter of exposing that existing function as a
callable global sub from within running code, rather than building REPL support from
scratch.

## Affected files (starting point)

- `src/repl.rs` (or wherever `run_repl()` lives) — the existing REPL entry point to
  reuse.
- Wherever builtin sub names are registered/dispatched (grep for how other
  environment-interaction builtins like `sleep`/`exit` are wired) — add `repl` as a
  builtin that calls into the same REPL loop, with access to the calling scope's
  lexicals if mutsu's REPL supports that (raku's `repl()` gives you an interactive
  prompt with the caller's lexical scope visible).
