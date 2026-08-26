# `do when COND { BLOCK }` used as an expression crashes, and yields the wrong no-match value

Discovered via the doc-diff harness on `raku-doc/doc/Language/control.rakudoc` (around line
526/537).

**Re-measured on `main` @ `17139dd55` against `raku` v2026.06 (2026-08-25). Both of the
expected values this ticket originally recorded have drifted, so the table below replaces
them — do not trust the doc's own narration here.**

**Update 2026-08-26: the no-match half is FIXED.** `news/2026-08/control-default-term-expression-position.md`
added `OpCode::PushWhenNonmatch`, which makes a non-matching `when` in expression position
yield the falsy smartmatch result that `exec_when_op` already recorded, instead of `Nil`/`Any`.
Both `False` rows now agree with raku. Only the **crash** rows are left, and they are a
different bug: an escaping `CX::Succeed` with no topicalizer to catch it.

## Measured behaviour

| case | raku v2026.06 | mutsu (2026-08-25) | mutsu (2026-08-26) |
| --- | --- | --- | --- |
| `$_ = True; my $a; { $a = do when .so { "foo" } }; say $a;` | `(Any)` | crashes: bare `Runtime error:` | crashes: bare `Runtime error:` |
| `$_ = False; my $a; { $a = do when .so { "foo" } }; say $a;` | `False` | `(Any)` | `False` (fixed) |
| `my $a; given True { $a = do when .so { "foo" } }; say $a;` | `(Any)` | `(Any)` (matches) | `(Any)` (matches) |
| `my $a; given False { $a = do when .so { "foo" } }; say $a;` | `False` | `(Any)` | `False` (fixed) |
| `$_ = True; my $a = do when .so { "foo" }; say $a;` | prints nothing at all | crashes: bare `Runtime error:` | crashes: bare `Runtime error:` |

The original ticket asserted that the matching case yields `foo`. It does not, in either
implementation: a matching `when` runs its block and then `succeed`s out of the enclosing
topicalizer, so the pending assignment never completes and `$a` keeps its declared default.
That is why the matching case is `(Any)` even in `raku`. The `given True` row shows mutsu
*already agrees* on the matching case when a real topicalizer is present.

## What is actually left to fix

**The crash, and only the crash.** With the topic set by plain `$_ = True` (no topicalizer),
a MATCHING `when` raises `succeed` with nothing to catch it, and mutsu surfaces the escaping
control signal as a bare `Runtime error:` with no message. Two rows above hit it. raku
absorbs it silently: the enclosing bare block (row 1) or the compilation unit itself (row 5)
simply ends, which is why row 1 prints `(Any)` and row 5 prints nothing at all.

The no-match value divergence this ticket also listed is gone — see the Update note above.

## Root cause (narrowed 2026-08-26, still needs the fix designed)

Not a `do`-value problem: `exec_when_op` already produces the right value on both paths. The
remaining bug is purely about **where an escaping `CX::Succeed` stops**. mutsu only catches it
at a topicalizer (`exec_given_op` / `exec_do_given_expr_op`). Raku additionally lets it be
absorbed by the nearest enclosing block, and by the unit at top level. So the fix is to give a
plain block boundary (and the unit) a terminal `succeed` absorber, rather than anything in the
`when` compilation.

That has to be done carefully — silently swallowing an escaping control signal is exactly the
kind of change that can mask a real bug — so it wants a deliberate decision about which
boundaries absorb, not a blanket catch at the top-level driver.

## Affected files (starting point)

- `src/vm/vm_given_when_ops.rs` — `exec_when_op` raises the `succeed` signal (correct today)
- `src/vm/vm_misc_block.rs` / `src/vm/vm_run_loop.rs` — block-boundary and top-level control
  signal handling, where the absorber would go
- `src/value/error.rs` — the bare-message `succeed_signal`, which is what surfaces as
  `Runtime error:` with no text

## Suggested next step

Run row 5 (`$_ = True; my $a = do when .so { "foo" }; say $a;`) under `rust-gdb -batch` with a
breakpoint on the top-level error reporter to confirm the escaping signal is the
`succeed_signal` and identify the last block boundary it passed through. Then decide which
boundary should absorb it; the `given False` row is already green and pins the value
semantics, so the change can be judged purely on the two crashing rows.
