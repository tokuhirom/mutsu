# `do when COND { BLOCK }` used as an expression crashes, and yields the wrong no-match value

Discovered via the doc-diff harness on `raku-doc/doc/Language/control.rakudoc` (around line
526/537).

**Re-measured on `main` @ `17139dd55` against `raku` v2026.06 (2026-08-25). Both of the
expected values this ticket originally recorded have drifted, so the table below replaces
them — do not trust the doc's own narration here.**

## Measured behaviour

| case | raku v2026.06 | mutsu |
| --- | --- | --- |
| `$_ = True; my $a; { $a = do when .so { "foo" } }; say $a;` | `(Any)` | crashes: bare `Runtime error:` |
| `$_ = False; my $a; { $a = do when .so { "foo" } }; say $a;` | `False` | `(Any)` |
| `my $a; given True { $a = do when .so { "foo" } }; say $a;` | `(Any)` | `(Any)` (matches) |
| `my $a; given False { $a = do when .so { "foo" } }; say $a;` | `False` | `(Any)` |
| `$_ = True; my $a = do when .so { "foo" }; say $a;` | empty line | crashes: bare `Runtime error:` |

The original ticket asserted that the matching case yields `foo`. It does not, in either
implementation: a matching `when` runs its block and then `succeed`s out of the enclosing
topicalizer, so the pending assignment never completes and `$a` keeps its declared default.
That is why the matching case is `(Any)` even in `raku`. The `given True` row shows mutsu
*already agrees* on the matching case when a real topicalizer is present.

## What is actually left to fix

1. **The crash.** With the topic set by plain `$_ = True` (no `given`), mutsu dies with a bare
   `Runtime error:` and no message, where `raku` completes. Two rows above hit it. A
   no-message runtime error is itself a bug regardless of the value semantics.
2. **The no-match value.** When the `when` does not match, `raku` evaluates `do when` to the
   *smartmatch result* (`False`), not to `Any`. mutsu gives `(Any)` in both the bare-block and
   the `given` form. This is the one genuine value divergence.

## Root cause (unconfirmed, needs a debugger session)

`do EXPR` normally evaluates `EXPR` and returns its value. mutsu's `when` is implemented as a
control-flow statement (`vm_control_ops.rs`) that assumes statement position inside a
`given`/loop body, so it has no value-producing path when wrapped in `do` — it appears to
substitute `Any` rather than the smartmatch result, and to have no handler at all when the
topic was set by assignment rather than by a topicalizer.

## Affected files (starting point)

- `src/compiler/expr.rs` / `src/compiler/stmt.rs` — wherever `do BLOCK`/`do STATEMENT` is
  compiled, to see how it handles a `when` operand
- `src/vm/vm_control_ops.rs` — `when` execution

## Suggested next step

Reproduce the crashing row under `rust-gdb` per `CLAUDE.md`'s debugging guidance to find which
opcode sequence `do when` compiles to and where the bare `Runtime error:` originates. Fix the
crash first; then make the no-match path yield the smartmatch result instead of `Any`, using
the `given False` row as the pin (it is the cleanest of the four, with a real topicalizer and
no crash involved).
