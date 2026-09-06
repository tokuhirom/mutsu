# A subscript receiver does not hand its element's container to a raw invocant

`@a[0].mut` and `%h<a>.mut`, where `mut` declares a raw invocant, must mutate
the element in raku. mutsu silently does nothing.

```raku
use v6.e.PREVIEW; use MONKEY-TYPING;
augment class Int { method mut(\S:) { S = 7 } }
my @a = 1, 2;  @a[0].mut;  say @a;   # raku: [7 2]   mutsu: [1 2]
my %h = a => 1; %h<a>.mut; say %h;   # raku: {a => 7} mutsu: {a => 1}
```

These are rows I3 and K3 of
[ADR-0067](../../docs/adr/0067-a-routine-hands-back-the-container-it-was-given.md).
Slice 3b made the *arrival* direction work — a raw invocant parameter now binds
the caller's container — but only for a receiver the call site can name.

## Root cause: `CallMethod` has no receiver location, and `Index` already read
## the value out

`--dump-bytecode` on `@a[0].mut`:

```
GetArrayVar(4); LoadConst(0); Index { is_positional: true }
CallMethod { name_idx: 3, arity: 0, modifier_idx: None, quoted: false, arg_sources_idx: None }
```

Two things follow. `CallMethod` carries no `target_name_idx` (only
`CallMethodMut` does), so slice 3b's arrival gate — which boxes the *named*
receiver's location with `capture_lvalue_invocant_cell` — has nothing to box.
And the `Index` op has already read the element's value onto the stack, so by
the time the method call runs there is no location left on it either.

The missing piece is a **producer**: the subscript must hand over the element's
own `Scalar` container rather than its value. The primitive exists —
`Value::array_slot_ref(i, true)` promotes an element to a shared cell in place
and idempotently (`vm_element_producers.rs` uses it for `.pairs`/`.values`/
`.Seq`), and the hash side has `HashEntryRef`.

## Why this is not a one-line change

Rawness is not statically known: `@a[0].mut`'s callee depends on the element's
runtime type and, for the dynamic spelling, on a runtime method-name string. So
whatever produces the container has to be emitted **unconditionally** for every
`<subscript>.method(...)` in every program — one of the most common shapes there
is — and then made invisible again at the consumer:

- Whatever `CallMethod` receives has to be decontainerized at a single
  chokepoint unless the resolved callee binds its invocant raw, or a
  `ContainerRef` leaks into every native and user method that matches
  `Instance`/`Array`/`Hash` on its invocant. This is the same hazard slice 3a
  had to solve at its own site (see the ADR's "One guard the boxing required").
- The gate cannot be the compiler's, so the runtime cost lands on a hot path
  and must be A/B measured the way slice 3a's and 3b's were. Slice 3b's
  process-global `any_raw_invocant_method_possible()` mirror is the right
  pre-gate to reuse.
- The JIT compiles `Index` and shims `CallMethod`, so whatever shape is chosen
  must behave identically under `MUTSU_JIT=on` — a producer that only fires in
  the interpreter would make the feature switch off once a loop goes hot, which
  is worse than not having it.

## Sibling

The attribute-accessor receiver (`$d.v.mut`, ADR-0067 row N1 / E6) is the same
missing-producer problem with a different producer (`MarkAccessorRefContext`),
and is tracked with slice 3a's E6 row. If both are done, they should probably
share the decontainerize chokepoint rather than each adding their own.

## Repro / pin

`t/raw-invocant-arrives-as-container.t` covers the shapes that do work. The two
programs at the top of this file are the repro; both are *silently* wrong today
(exit 0, no diagnostic), which is what they were before slice 3b too — this
ticket is a missing capability, not a regression.
