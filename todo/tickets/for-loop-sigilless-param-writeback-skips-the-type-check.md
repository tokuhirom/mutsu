# A `for`-loop's sigilless parameter writes back to its source without type-checking

*(Rewritten 2026-09-04. The `:=`-alias half of this ticket is fixed — see
`news/2026-09/an-alias-of-an-undefined-scalar-writes-through.md`. What is left
is the loop-parameter spelling, which uses a different mechanism.)*

## The divergence

```
subset SmallInt of Int where -128 <= $_ <= 127;
my SmallInt $a;
for $a -> \x { x = 1000 }
say $a;
```

- raku: `Type check failed in assignment to $a; expected SmallInt but got Int (1000)`
- mutsu: silently sets `$a` to `1000`

The write-through itself is correct (fixed by
`news/2026-09/for-list-multi-param-source-writeback.md`); only the check is
missing. The `:=` spelling of the same thing now behaves — `my SmallInt $a; my
\x := $a; x = 1000` throws `X::TypeCheck::Assignment` — so this is specific to
how a loop parameter reaches its source.

## Why the alias fix does not cover it

A `:=` bind promotes the source into a shared `ContainerRef` cell and tags the
cell with the source's `of`-type (`register_container_constraint_named`), so
every later write through either name re-checks it at the container chokepoint —
that is ADR-0042's "the constraint belongs to the container" rule.

A `for` loop parameter over a *scalar* source does not bind that cell. It binds
the item value and relies on a **source-variable writeback**:
`write_back_to_source_var` → `store_loop_source_var`
(`src/vm/vm_loop_writeback_quant.rs`), which writes straight into `env` and the
local slot:

```rust
self.env_mut().insert(target.to_string(), value.clone());
match source_var_locals.get(idx).copied().flatten() { Some(slot) => … }
```

No constraint lookup, no chokepoint — so nothing can check it.

## Two ways to fix it, and why neither is a one-liner

1. **Type-check inside the writeback.** `store_loop_source_var` would consult
   `var_type_constraint(target)` and raise `X::TypeCheck::Assignment`. But it
   and its four callers all return `()`, across 11 call sites reached from
   inside the loop's control flow (`vm_for_loop_body.rs`,
   `vm_for_loop_intrange.rs`, `vm_loop_writeback_quant.rs`), so it needs `Result`
   threaded through the writeback path — with real regression risk in `next`/
   `last`/`redo` handling.

2. **Make the loop parameter a real alias** of the source variable's container,
   the way `:=` does, and delete the scalar-source writeback for that case. Then
   the existing container chokepoint checks it for free, and one mechanism
   replaces two. This is the shape ADR-0045 points at and the better answer, but
   it is an ADR-0045 slice, not a ticket-sized change.

Prefer (2); do not do (1) as a stopgap without recording why.

## Provenance and the named consumer

Split out of `todo/deep/for-loop-pointy-sigilless-param-write-through-missing.md`
on 2026-09-02 when its write-through half was fixed. `Native::Overflow`'s
`t/01-basic.rakutest` is the consumer, and it uses the **loop** spelling
(`for LIST -> \x, $value { x = … }`, per
`news/2026-08/sigilless-alias-write-now-type-checked.md`), so it is still blocked on this
half.

## Also still open in the same area

A two-hop sigilless bind chain rejects the write:

```
$ raku  -e 'my $a = 1; my \y := $a; my \x := y; x = 5; say $a'
5
$ mutsu -e '...same...'
Cannot modify an immutable Int (1)
```

Same family (the second bind does not reach the first alias's cell), listed in
`news/2026-08/sigilless-alias-write-now-type-checked.md`'s "what is still open" and
unaffected by the 2026-09-04 fix.

## Reproduce

The snippets above, no fixtures.
