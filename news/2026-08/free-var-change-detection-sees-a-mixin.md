# A closure's writeback could not see that `$x does R` changed anything

A closure's caller-writeback decides which captured free variables to propagate
by comparing each one's value at entry with its value at exit:

```rust
.filter(|(k, old)| self.env().get_sym(**k) == old.as_ref())   // "unchanged"
```

Plain `==` is the wrong test. `Value`'s equality is Raku's **semantic**
equality, and a `does` mixin is equal to the value it wraps. So `$x does R`
inside a block left the comparison reporting

```
[CWB] r entry=Hash now=Mixin changed=false
```

the name landed in `unchanged_free`, and the writeback skipped it. The caller
kept the un-mixed value:

```raku
sub call1(&b) { b() }
my $a = {:x};
call1 { $a does role { has $.tag = "tagged" } };
say $a.tag;     # raku: tagged      mutsu: No such method 'tag' for invocant of type 'Hash'
```

`lives-ok { $a does R }` is the shape that hides it: the block runs through a
routine, so the writeback is the only thing that can carry the mixin back. A
`does` in a *bare* block, or at file scope, was always fine — no writeback
involved.

The three comparisons that ask this question (`free_changed`, `unchanged_free`,
and the `pending_rw_writeback_sources` recording) now go through one
`free_var_changed`, which checks the `ValueView` **discriminant** before falling
back to `==`. A change of representation is a change even when the two compare
equal; that can only add writebacks the old test missed, and each of them is a
genuine mutation.

`roast/S14-roles/anonymous.t` (13) and `roast/S14-roles/parameterized-mixin.t`
(28) now pass under `MUTSU_REAL_TEST=1`; both were aborting on their fourth
assertion.

## Three earlier hypotheses, all wrong

This one took four attempts, and the three that failed are worth recording
because each was plausible:

1. **The lexical is not boxed into a `ContainerRef` cell.** True, but by design
   — an immediately-invoked call argument is deliberately excluded from the
   boxing gate for perf. Not the cause.
2. **`OpCode::DoesVar` is missing from the by-name write classification.**
   Adding it to `op_name_const_idx` / `op_name_write_const_idx` changed nothing
   observable: the closure's `free_var_syms` and `free_var_writes` were already
   byte-identical between the working assignment case and the failing `does`
   case. Measured, then reverted.
3. **`exec_does_var_op` uses a raw `env.insert` instead of the by-name store.**
   Also true, also a real bug — `news/2026-08/does-writes-through-the-assignment-store.md`
   fixes a compunit-lexical case with it — but it did not fix this one.

What found it was instrumenting the *comparison* rather than the write: printing
`entry=` / `now=` / `changed=` for each free var at the writeback showed a
`Hash` becoming a `Mixin` and being called unchanged. **When a value provably
reaches a place and still has no effect, instrument the predicate, not the
store.**

Pin: `t/does-mixin-reaches-the-caller.t` — without the fix it dies before its
first assertion; all 9 pass under `raku`. It also pins the shapes that already
worked (bare block, file scope) and the writeback cases the widened comparison
could have disturbed (a same-valued write, an ordinary change, a read-only
capture).
