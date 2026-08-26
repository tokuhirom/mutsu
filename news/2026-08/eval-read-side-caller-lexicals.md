# EVAL now sees the caller's current lexical values, not just writes

`EVAL` already let an EVAL'd string *write* a caller's lexical (`EVAL '$x = 7'`
after this returns, the caller's `$x` really is `7`), but *reading* an
already-declared caller lexical silently came back as a stale placeholder in
one specific, extremely common shape:

```
$ mutsu -e 'my $x = 5; EVAL q[say $x]; EVAL q[$x = 7]; say $x;'
(Any)     # raku: 5
7         # raku: 7
```

The first line printed `(Any)` where raku prints `5` — the read failed, while
the write on the next line already worked.

## Root cause

`EVAL` compiles the string it is given into a fresh, separately-compiled unit
and runs it via `eval_block_value`/`run_compiled_block` on the same
`Interpreter`. That fresh unit has no compile-time knowledge of the caller's
local variable *slots* — it resolves every caller lexical by NAME against
`Interpreter::env` instead.

A plain lexical's slot only mirrors its value into `env` on every write once
the process-global, monotonic `REFLECTIVE_NAME_ACCESS_SEEN` flag has latched
(`crate::opcode::reflective_name_access_possible`, consulted by
`vm_var_assign_set_local.rs`'s `skip_env_write` gate). Without it latched, a
"slot-only" plain lexical never reaches `env` at all — a perf optimization
(`docs/vm-dual-store.md`) that assumes nothing needs to read that lexical by
name.

The flag latches during compile-time finalization
(`CompiledCode::scan_reflective_name_access` in `opcode.rs`), which scans a
chunk's compiled ops for `EVAL`/`EVALFILE` calls (among other reflective
constructs like `CALLER::`/`OUTER::`). That scan recognized only the
tail/expression call shapes `OpCode::CallFunc`/`CallFuncNamed` — but `EVAL
'...';` written as a bare statement (its return value discarded, which is how
almost every real-world `EVAL '...';` is written) compiles to
`OpCode::ExecCall`, or `OpCode::ExecCallPairs` when it carries named arguments
like `:lang`. Neither of those was in the scan's match arm. So a program whose
only `EVAL` calls were bare statements never latched the flag at all: every
plain lexical it might read stayed unmirrored in `env`, and `EVAL` read back
the placeholder `Any` the declaration seeded rather than the live value.

The write side worked regardless, because it goes through a separate,
already-correct mechanism (`writeback_carrier_writes`, driven by
`begin_carrier`/`end_carrier`) that reconciles whatever `EVAL` wrote into
`env` back into the calling frame's own local slots after the call returns —
independent of whether that frame's own writes were mirrored going in.

## Fix

`scan_reflective_name_access` now also recognizes `OpCode::ExecCall` and
`OpCode::ExecCallPairs` (`src/opcode.rs`), so any `EVAL`/`EVALFILE` call —
regardless of whether its result is used — latches the flag. This is a small
extension of the existing, already-sound mechanism (no new dual-store
machinery, no special-casing of the read path), so it composes with every
other consumer of the flag automatically.

## What was measured against raku

Before implementing, the following axes of EVAL's caller-lexical read
visibility were measured against `raku` directly (not assumed):

- Reading a `my` lexical from the immediately enclosing scope, and from
  several enclosing routine frames up: both now work.
- A `my` declared *inside* the EVAL'd string does not leak into (or overwrite)
  a same-named caller lexical — confirmed unaffected by this fix (was already
  correct, and stays correct).
- `our`/package vars, `state` vars, and the topic `$_`: all read correctly,
  both before and after this fix in the cases they didn't already hit the
  gap.
- Sigilless reads work the same via `@`/`%`/`&` sigils (array/hash/code-var
  lexicals), not just `$`.
- A closure created *inside* the EVAL'd string that captures a caller
  lexical, called after the EVAL returns and after a further external
  mutation of that lexical, correctly tracks the live value (was returning a
  stale `Any` before this fix; now matches raku).
- `EVAL` with `:lang<Raku>` (a named-arg call site, `ExecCallPairs`) and a
  nested `EVAL` inside an EVAL'd string both read the outer caller's lexical
  correctly.

One narrower, pre-existing, and unrelated gap was found and intentionally
left out of scope: reading a `my` declared *textually after* the `EVAL` call
in the same block raises `Variable ... is not declared` in mutsu, where raku
resolves it to `(Any)` (its lexical pad slot exists but is not yet
initialized). This is a compile-time lexical-hoisting difference, not a
caller-lexical-*visibility* gap, and is unaffected by this change.

## Regression test

`t/eval-read-caller-lexicals.t` pins the fix with 16 subtests, deliberately
written so every `EVAL` call in the file is a bare statement (never assigned,
never a call argument, never a block's tail expression) — the exact shape
that exposed the gap — and so every closure the file needs is anonymous
(`my &f = sub (...) {...}`), never a named `sub f(...) {...}` declaration,
since a named sub triggers a separate, unrelated "unknown free variables"
conservative-sync rule that would otherwise mask this specific bug. The file
passes under both `raku` and mutsu.
