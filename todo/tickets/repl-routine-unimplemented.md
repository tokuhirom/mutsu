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

## Why it is deferred rather than implemented (updated 2026-08-26)

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
scope — was blocked on a live mutsu gap, now fixed: `EVAL` used to see caller
lexicals for **writes** but not for **reads**:

```
$ mutsu -e 'my $x = 5; EVAL q[say $x]; EVAL q[$x = 7]; say $x;'
(Any)     # raku: 5    -- was the bug; mutsu now also prints 5
7         # raku: 7
```

**Status: that blocker is cleared.** Root cause and fix are recorded in
`news/2026-08/eval-read-side-caller-lexicals.md` — in short, `EVAL` resolves
caller lexicals by NAME against `Interpreter::env`, and a plain lexical's slot
only mirrors into `env` once the process-global `REFLECTIVE_NAME_ACCESS_SEEN`
flag has latched (`crate::opcode::reflective_name_access_possible`). The
compile-time scan that latches it (`scan_reflective_name_access` in
`opcode.rs`) used to recognize only the tail/expression call shapes
(`CallFunc`/`CallFuncNamed`); a bare statement `EVAL '...';` (the overwhelmingly
common shape — its value discarded) compiles to `ExecCall`/`ExecCallPairs`,
which the scan never matched. Fixed by adding those two opcodes to the scan.
Regression coverage: `t/eval-read-caller-lexicals.t` (16 subtests spanning
`our`/`state`/topic/sigilless/`@`/`%`/`&`/nested-closure/`:lang`/nested-EVAL
reads, plus the no-leak negative case), passing under both `raku` and mutsu.

**`repl()` itself is still unimplemented** — this ticket now covers only that.
What's left to actually write it:

1. Wire `repl` into the builtin/name tables (`src/runtime/builtins.rs`,
   `src/runtime/system_eval_names.rs`, `src/parser/primary/ident/predicates.rs`),
   dispatching to a routine that runs each line through the same `EVAL`
   machinery `builtin_eval` in `src/runtime/builtins_eval_misc.rs` uses, so it
   inherits the caller's lexical scope for both reads and writes (now that both
   directions work).
2. mutsu's existing interactive REPL (`src/repl.rs`/`src/repl_core.rs`) is
   gated behind the `native` feature's rustyline loop. `repl()` must work in a
   build without that feature too (or without a TTY at all — piped stdin),
   since it is an ordinary Raku statement a script can call from any build.
   Decide the non-interactive behavior (e.g. read lines from `$*IN` until EOF,
   `EVAL`-ing each one in the caller's scope, printing results the way the
   real REPL echoes non-`;`-terminated expressions) and add a piped-stdin test
   for it — do not assume a TTY is present.
3. `repl_core::process_line`'s existing parsing/echo logic is a useful
   reference for the piped-stdin behavior, but it currently runs against a
   fresh `Interpreter`; the `repl()` builtin must instead reuse `EVAL` against
   `self` (the caller's own live interpreter) so declarations and mutations
   the REPL session makes are visible to `say "Goodbye, $name"` after the
   session ends, per the repro at the top of this file.

## Affected files (starting point)

- `src/runtime/builtins_eval_misc.rs` — `builtin_eval`; the routine `repl()`
  should reuse (`eval_eval_string`/`eval_block_value`) rather than
  reimplementing scope handling
- `src/repl.rs` / `src/repl_core.rs` — the existing (native-feature-gated,
  fresh-`Interpreter`) REPL loop; useful as a reference for echo/parse
  behavior, not directly reusable as-is (see point 3 above)
- `src/runtime/builtins.rs`, `src/runtime/system_eval_names.rs`,
  `src/parser/primary/ident/predicates.rs` — where the sub would be registered
