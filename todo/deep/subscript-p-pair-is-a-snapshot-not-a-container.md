# `@a[0]:p` and `.pairs` yield snapshot Pairs — designed in ADR-0036, implementation open

**Status (2026-08-20): still open, but the design pass is done — the analysis now lives in
[docs/adr/0036-element-container-pairs-from-subscripts-and-pairs.md](../../docs/adr/0036-element-container-pairs-from-subscripts-and-pairs.md).**
Start there, not here. This file stays only so the finding remains discoverable from `todo/deep/`;
it is removed when ADR-0036 slice 2 lands.

## What the ADR settled

Re-measured against `main` at `e13d278ff`, the finding is real and materially larger than this file
originally described. Raku's `@a[0]:p` is `0 => @a[0]` where the value **is** the element's `Scalar`
container; mutsu builds the Pair from a clone and then searches `self.env` at assignment time for an
array/hash whose element compares equal. That produces twelve measured divergences in three classes
(ADR-0036 §1.3): stale *reads* through the pair (which no search can ever fix), ambiguity failures
that a plain `my @b = @a;` anywhere in scope is enough to trigger — dying with a misleading
`X::Assignment::RO ... on non-instance` on the `:p` path and *silently doing nothing* on the
`.pairs` path — and skipped enforcement (a `List` source that should be read-only, and a typed
array's element constraint).

## The one claim in the original write-up that was wrong

This file previously concluded that "there is no `array_element_cell`-style API today" and that the
fix therefore waits on ADR-0001's Track B / the GC campaign. **That is stale.** The element-container
primitive shipped and is in daily use on the `:=` binding path: `Value::array_slot_ref`
(`src/value/value_methods_b.rs:94`) and `Value::hash_slot_ref` (`src/value/value_methods_a.rs:603`)
promote an element in place to a shared `ContainerRef` cell and return that cell, and
`resolve_array_entry` (`src/vm/vm_var_ops.rs:147`) decontainerizes it on read. `my $r := @a[0]` gets
write-through, read-through, no ambiguity, and full invisibility to `.raku`/`.elems`/copy — all
verified. What is missing is that the pair *producers* never ask for the slot.

So the work is routing over an existing primitive, not building one, and ADR-0036 scopes it as five
landable slices with the mutability of the source as the discriminator.

## Repro

Any row of ADR-0036 §1.3. The shortest:

```raku
my @a = <A B>; my @b = @a; (@a[0]:p).value = "z"; say @a;   # raku: [z B]   mutsu: dies
my @a = <A B>; my $p = @a[0]:p; @a[0] = "Q"; say $p.value;  # raku: Q       mutsu: A
```

The original repro — a bare block whose sibling redeclares `@a`, flipping the first block to
`BlockScope` so `@a` lives in a local slot the env scan cannot see — still reproduces verbatim, but
it is one special case of the ambiguity class, not the defect itself.
