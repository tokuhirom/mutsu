# The `is <Type>` capture-cell exclusion is by NAME across the whole frame

Narrowed 2026-09-07. This file used to cover the whole "a typed container
capture loses to a same-named caller array" family; the element-constraint half
(`my Int @a`, `my Str %h`) is fixed in
`news/2026-09/element-typed-containers-take-the-capture-cell.md`. What is left
is the mechanism that half turned out NOT to need: the `ApplyVarTrait` name
scan in `CompiledCode::compute_free_vars`, which the CONTAINER type traits
(`my %h is BagHash`) genuinely do need and which is coarser than it should be.

## Repro

```raku
{
    my %h = a => 1, b => 2, c => 3;
    my $f = -> { %h<d> = 4; %h.elems };
    sub c1() { my %h = z => 9; $f.() }
    say c1();
}
{
    my %h is BagHash = q => 1;   # a DIFFERENT block, a different binding
}
# raku:  4
# mutsu: 2
```

Delete the second block and mutsu answers 4 too. The `is BagHash` declaration
opts *every* `%h` in the frame out of the capture cell, because same-named `my`
locals share one slot and the scan is therefore by name rather than per
declaration. The mutating capture in the first block then has neither defence —
no vouch (it writes) and no cell — so the caller's `%h` wins.

## Why the scan exists

`my %h is BagHash = a => 1, b => 0, c => 2` builds a plain `Hash` at the
declaration store and lets `ApplyVarTrait` coerce it to the QuantHash
afterwards, reading the slot back to find the initial values. A `ContainerRef`
in that slot is not the `Hash` it looks for, so the initialiser is silently
discarded and `%h` comes out with one key instead of two. Measured again
2026-09-07: removing the scan fails 39/344 subtests in
`roast/S02-types/baghash.t` and 40/295 in `mixhash.t`. It is load-bearing
exactly as written.

## The fix

Make the trait-application path see through the cell, so the scan is no longer
needed at all: `src/vm/vm_var_trait_ops.rs` reads the declared value with
`read_local_slot_or_name` (`src/vm/vm_env_helpers.rs`), which hands back
whatever is in the slot — a `ContainerRef` included — and writes the coerced
value back with `write_local_slot_or_name`, which REPLACES the slot and so
would drop the cell. Both ends have to move together: deref on the way in,
write through the cell on the way out.

That is a small change in one place, but `exec_apply_var_trait_op` classifies
eight value shapes (`Hash`, `Array`, `Seq`, `Slip`, `LazyList`,
`Set`/`Bag`/`Mix`, scalar, `Nil`/`Package`) and each arm has to be re-checked
against a celled input, plus the same question asked of the other `is <Type>`
consumers (`register_var_container_type_metadata`, `tag_container_metadata`,
`attr_build_defaults.rs`). Worth doing deliberately with the QuantHash roast
files as the gate, not as a rider on another change.

## Acceptance

The repro prints `4`; `roast/S02-types/{baghash,mixhash,bag,set,mix,sethash}.t`
still pass; the `ApplyVarTrait` subtraction in
`CompiledCode::compute_free_vars` is deleted; and
`t/typed-container-capture-cell.t` and `t/container-capture-cell-dichotomy.t`
still pass, with the repro above added to one of them.
