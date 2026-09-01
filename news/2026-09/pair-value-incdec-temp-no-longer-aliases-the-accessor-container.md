# `$p.value++` in a loop no longer stalls and then hangs

A Pair whose value was captured from a container (`n => $n`) supported
`.value++` correctly once, but inside a `for` or `while` loop the second
increment was a no-op and the third wedged the interpreter:

```
$ raku  -e 'my $n = 0; my $r = (n => $n); for 1..3 { $r.value++; say $r.value }'
1
2
3
$ mutsu -e 'my $n = 0; my $r = (n => $n); for 1..3 { $r.value++; say $r.value }'
1
1
<hangs; under prove, "thread 'mutsu-main' has overflowed its stack">
```

Only the postfix/prefix `++`/`--` forms inside a loop body failed. Three
separate `$r.value++` statements, the same increment inside a sub called three
times, and `+=` or an explicit read-add-store inside the loop were all correct
— which is what pointed at the lowering rather than the writeback.

## Root cause: the scratch temp was an alias, not a snapshot

`compiler/expr_postfix.rs` lowers `$r.value++` into: read the accessor,
`SetGlobal` the value into a compiler-synthesized temp global
(`__mutsu_tmp_method_inc_<N>`), bump that temp, then call
`__mutsu_assign_method_lvalue` to write it back through the accessor. The temp
is meant to be a scratch *value* slot.

It was not. The accessor read hands back the target's own container here — a
Pair built as `n => $n` holds `$n`'s cell — so:

1. `SetGlobal` bound the temp to that `ContainerRef`.
2. `PostIncrement`'s `ContainerRef` arm (`vm/vm_var_assign_post_incdec.rs`)
   dereferenced the cell, incremented it **in place**, and returned early
   *without rebinding the temp*. Correct for a variable genuinely bound to a
   container (`$!attr := $outer`); wrong for a scratch slot.
3. On the next iteration the temp was therefore still bound to the cell, and
   `SetGlobal` on a name whose env entry is a `ContainerRef` **writes through
   it**. So the freshly read cell was stored *into that same cell*: the
   container now pointed at itself.

That is the whole progression. Iteration 1 worked because the temp was still
unbound, so `SetGlobal` bound rather than wrote through. Iteration 2 created
the self-reference, and the increment it computed was the stale one. Iteration
3 read through the self-referential cell and recursed until the stack ran out
— a stack overflow on a *read*, which is why the symptom looked like a hang
rather than a wrong answer.

Measured with a single `rust-gdb -batch` run over the repro, breaking on the
cell store in the Pair `.value` lvalue arm: hits 1 and 2 both stored the same
`Int`, hit 3 stored a pointer-tagged `ContainerRef`.

## The fix

A new `OpCode::DerefContainer` reads a `ContainerRef` on the stack top through
its cell (a no-op for everything else), and all four inc/dec method-lvalue
lowerings emit it between the accessor read and the `SetGlobal` into the temp.
The temp then holds a plain value snapshot, so it can never be bound to a
container, `PostIncrement` bumps the temp instead of the source, and the single
write reaches the accessor through the writeback call — exactly once.

The opcode is in the JIT's Tier A step-shim list next to `Decont`, so a hot
loop containing `$obj.attr++` still compiles rather than bailing out to the
interpreter.

Pinned in `t/pair-value-incdec-in-loop.t` (14 tests: postfix and prefix, `++`
and `--`, `for` and `while`, the old-value/new-value contract, an ordinary
`is rw` attribute accessor, a mutable `BagHash` weight, and a literal Pair
which must still be refused rather than incremented).
`t/lvalue-method-writeback-coherence.t`'s loop block, which had to be written
as `$r.value = $r.value + 1` to dodge this, is back on `$r.value++`.
