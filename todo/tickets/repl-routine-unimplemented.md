# `repl()` global routine is unimplemented — and blocked on EVAL's caller-lexical visibility

Found by the doc-diff harness (`docs/doc-diff-backlog.md`,
`Type/independent-routines.rakudoc:148`).

## Repro

```raku
my $name = "Alice";
say "Hello, $name";
repl();
say "Goodbye, $name"
```

- `raku`: runs, drops into an interactive REPL at `repl()`, then continues after
  the user exits it.
- `mutsu` (`target/debug/mutsu`): fails to compile —
  `===SORRY!=== Error while compiling ...\nUndeclared routine:\n    repl used`.

## Membership verdict: core

Both halves of `CLAUDE.md`'s test pass. `raku -e 'sub f() { say repl.^name }'`
resolves `repl` with no `use`, and it is documented as `sub repl()` in
`raku-doc/doc/Type/independent-routines.rakudoc:134`. So it does belong in core
eventually.

## Why it is deferred rather than implemented (measured 2026-08-26)

The ticket originally read "mutsu already has the REPL machinery to reuse —
`mutsu::repl::run_repl()`, wired up in `src/main.rs`". That machinery is not
reusable for `repl()`, and shipping it anyway would be a stub.

The doc is explicit about what `repl()` is *for*: "This REPL is exactly like the
one created when you run `raku` without any arguments **except that you can
access/modify the program's current context (such as lexical variables)**." The
whole point is the caller's lexical scope. `run_repl()` constructs a *fresh*
`Interpreter` and drives `repl_core::process_line`, which calls
`interpreter.run(source)` — a full compile-and-run of a new program. It has no
access to a running frame's lexicals at all.

The obvious alternative — evaluate each REPL line with `EVAL` in the caller's
scope — is blocked on a live mutsu gap. `EVAL` sees caller lexicals for
**writes** but not for **reads**:

```
$ mutsu -e 'my $x = 5; EVAL q[say $x]; EVAL q[$x = 7]; say $x;'
(Any)     # raku: 5
7         # raku: 7
```

So a `repl()` built on `EVAL` would greet the user with `(Any)` for every
variable they asked about — worse than not having the routine, because it looks
like it works. A `repl()` built on `run_repl()` would silently be a scratch
prompt with none of the calling context.

**Implement the EVAL read-side lexical visibility first**; `repl()` is a thin
wrapper on top of it and is not worth attempting before then. Note also that
mutsu's REPL is the `native` feature's rustyline loop, so `repl()` would need a
sensible non-interactive answer (and a piped-stdin test) as well.

## Affected files (starting point)

- `src/runtime/builtins_eval_misc.rs` — `builtin_eval`, the read-side scope gap
  that is the actual blocker
- `src/repl.rs` / `src/repl_core.rs` — the existing REPL loop, reusable only
  once the above is solved
- `src/runtime/builtins.rs`, `src/runtime/system_eval_names.rs`,
  `src/parser/primary/ident/predicates.rs` — where the sub would be registered
