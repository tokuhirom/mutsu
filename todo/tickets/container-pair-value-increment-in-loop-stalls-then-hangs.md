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

## Where to look

`src/runtime/methods_mut_method_lvalue.rs`, the `ContainerRef` arm at the top
of the Pair `.value` lvalue path (it writes `*cell.lock().unwrap() = value`),
together with whatever computes the incremented value for a postfix
`++`/`--` on a method lvalue. The suspicion is that the "old value" read for
the increment yields the `ContainerRef` itself rather than its contents, so the
store writes the cell into the cell; the first iteration works because the
loop's first entry still sees a plain value.

`rust-gdb -batch` breaking on that store arm and printing whether the incoming
`value` is a `ContainerRef` should settle it in one run — see the debugging
guidelines in CLAUDE.md; do not reach for `eprintln!` first.

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
