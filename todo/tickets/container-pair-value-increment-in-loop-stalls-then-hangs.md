# `$p.value++` on a container-backed Pair stops accumulating in a loop, then hangs

## Symptom

A Pair whose value was captured from a container (`n => $n`) supports
`.value++` correctly once, but inside a `for` loop the second increment is a
no-op and the third one wedges the interpreter:

```
$ raku  -e 'my $n = 0; my $r = (n => $n); for 1..3 { $r.value++; say $r.value }'
1
2
3
$ mutsu -e 'my $n = 0; my $r = (n => $n); for 1..3 { $r.value++; say $r.value }'
1
1
<hangs; under prove it aborts with "thread 'mutsu-main' has overflowed its stack">
```

Two iterations stall silently; the third loops/recurses forever. The shape of
the failure (a stack overflow on a *read*) points at the cell ending up
holding a reference to itself, so dereferencing it never terminates.

It is specific to the postfix form inside a loop:

| shape | result |
| --- | --- |
| `$r.value++` outside a loop, twice | correct (`2`) |
| `for 1..1 { $r.value++ }` | correct (`1`) |
| `for 1..2 { $r.value++ }` | wrong (`1`, second increment lost) |
| `for 1..3 { $r.value++ }` | hangs / stack overflow |
| `for 1..3 { $r.value = $r.value + 1 }` | correct (`3`) |
| `for 1..3 { $r.value-- }` (from 5) | hangs the same way |

The explicit read-add-store form is the tell: only the postfix
increment/decrement path is broken, and only when re-entered from a loop body.

## Pre-existing, and not the Pair immutability guard

Found on 2026-09-01 while correcting `t/lvalue-method-writeback-coherence.t`
for the `Pair.value` immutability guard
(`todo/tickets/pair-value-assign-does-not-enforce-immutable-value.md`). It
reproduces with that guard reverted, so it is independent of it. The reason it
was not visible before is that the file's loop block used a *literal* Pair
(`my $r = n => 0`), which took the standalone-pair env-rebind compensator that
the guard has since replaced with `X::Assignment::RO`; the compensator built a
fresh `Pair` each time and so never touched a cell. Converting the block to the
container form — the only form raku accepts — is what exposed this.

## Measured 2026-09-01 (`rust-gdb`, no rebuild)

Breaking on the `ContainerRef` store in the Pair `.value` lvalue arm
(`methods_mut_method_lvalue.rs`, `*cell.lock().unwrap() = value.clone()`) for
`for 1..3 { $r.value++ }` shows exactly three hits, and the third is the fatal
one:

| hit | stored `value` (NaN box) | reading |
| --- | --- | --- |
| 1 | `281474976710657` | a small `Int` — `1` |
| 2 | `281474976710657` | the **same** `Int` `1` |
| 3 | `18444633010847552579` | pointer-tagged — a **`ContainerRef`** |

So there are two distinct faults, in order:

1. **The read is stale.** Iteration 2 computes `0 + 1` again rather than
   `1 + 1`, which is why the weight never advances past `1`. The cell already
   held `1`, so `$r.value` inside the loop body is not reading the live cell.
2. **The third read yields the cell itself**, and storing it into that same
   cell makes it point at itself. Every later deref then recurses forever —
   the "stack overflow on a read" the symptom section describes.

Narrowing rules out the obvious suspects: it is the `++`/`--` form **inside a
loop body** specifically, not repeated execution and not the writeback:

| shape | result |
| --- | --- |
| `$r.value++` three times as separate statements | correct (`3`) |
| `sub f() { $r.value++ }; f(); f(); f()` | correct (`3`) |
| `for 1..3 { $r.value += 1 }` | correct (`3`) |
| `for 1..3 { $r.value = $r.value + 1 }` | correct (`3`) |
| `for 1..3 { $r.value++ }` | stalls, then hangs |
| `while $i < 3 { $r.value++; $i++ }` | stalls, then hangs |

A sub call reuses the same temp global name and works, so a bare
"global temp collides across calls" story does not explain it on its own; what
distinguishes the failing rows is the loop body's env/locals handling.

## Where to look

`src/compiler/expr_postfix.rs`'s `MethodCall` arm of the postfix `++`/`--`
compiler. It lowers `$r.value++` into: read the accessor, `SetGlobal` the value
into a temp global (`__mutsu_tmp_method_inc_<N>`), `PostIncrement` that global,
then call `__mutsu_assign_method_lvalue` with `Expr::Var(tmp)` as the new value.
The two temp globals are named from `self.code.constants.len()` at compile time,
so a loop body reuses the same two names on every iteration while the loop's
env/locals handling decides what a re-read of `$r` and a re-`SetGlobal` of an
already-populated global actually do. That is the interaction to instrument:
whether `SetGlobal` on a global already holding a `ContainerRef` rebinds the
name or writes *through* the cell, and whether the loop body re-reads `$r` or a
snapshot taken at loop entry.

Note that `+=` on the same lvalue compiles through
`method_lvalue_roundtrip_assign_expr` (`compiler/compound_expr.rs`) and is
correct in all the same shapes, so that path is the working reference to diff
the postfix lowering against.

Keep using `rust-gdb -batch` rather than `eprintln!` — see the debugging
guidelines in CLAUDE.md; the table above was produced in a single run with no
rebuild.

## Related

A second, separate container leak shows up in the same block and is *not* this
bug: a plain `$r.value` read hands the caller the cell, so
`@log.push($r.value)` aliases every pushed element to the final value
(`[3 3 3]` where raku gives `[1 2 3]`). That is the read-boundary problem
already tracked in
`todo/deep/pairs-element-containers-leak-through-pair-value-consumers.md`;
`.Int` decontainerizes as a workaround, which is what the test now does.

## Repro to pin when fixed

```raku
my $n = 0;
my $r = (n => $n);
my @log;
for 1..3 { $r.value++; @log.push($r.value.Int) }
is-deeply @log, [1, 2, 3], '.value++ accumulates across loop iterations';
is $n, 3, 'and the increments reached the source container';
```
