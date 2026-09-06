# A TYPED container capture still loses to a same-named caller array

Measured 2026-09-06 while landing the container half of ADR-0055's capture cell
dichotomy (`news/2026-09/container-captures-join-the-cell-dichotomy.md`). Every
other shape in that family is fixed; a container carrying an element type
constraint or an `is <Type>` trait is the remaining hole.

## Repro

```raku
my Int @a = 1, 2;
@a.push(3);
my $f = -> { @a.elems };
sub collide() { my Int @a = 9; $f.() }
say collide();          # raku: 3    mutsu: 1
```

Drop the `Int` and mutsu answers 3. The same holds for `my Str %h`, and for the
`is <Type>` container traits (`my %h is BagHash`, `is SetHash`, `is Buf`, ...).

## Root cause

`needs_cell_unvouched_containers` is delivered at the declaration site by
`box_decl_local_container_cell`, which refuses a typed container because "typed
containers must keep flowing through the assignment chokepoint", and
`compute_free_vars` additionally subtracts every name an `ApplyVarTrait` op in
the frame names. Both refusals were measured, not assumed:

- Lifting the typed refusal for this trigger made `my %h is BagHash = a => 1,
  b => 0, c => 2` initialise to ONE key instead of two, and dropped 24 subtests
  across `roast/S02-types/baghash.t` and `mixhash.t`. `ApplyVarTrait` reads the
  declared slot back to find the initial values it must coerce into the
  QuantHash, and a `ContainerRef` in that slot is not the `Hash` it looks for,
  so the initialiser is silently discarded.
- The `ApplyVarTrait` subtraction is by NAME across the whole frame (same-named
  `my` locals share one slot), so one `my %h is BagHash` anywhere in a file opts
  every other `%h` in that frame out of the cell too.

The element-constraint case (`my Int @a`) was measured to survive the cell —
ADR-0042 made the constraint a property of the container, so `@a.push("x")`
through a cell still throws `X::TypeCheck::Assignment`. It is refused only
because it shares `box_decl_local_container_cell`'s single typed check with the
`is <Type>` case, which does not survive.

## Why this is not a one-liner

Splitting the two would mean deciding, at the boxing site, whether a name's
constraint is an *element* type (survives the cell) or a *container* type (does
not) — and the two are not distinguished there today: `is BagHash` is not even
visible through `var_type_constraint`, which is why the compile-time
`ApplyVarTrait` scan exists at all.

The better fix is at the other end: make the trait-application path
(`vm_var_trait_ops.rs`, via `read_local_slot_or_name`) deref a `ContainerRef`
before it classifies the declared value, so a celled slot is no longer invisible
to it. That is a small change in one place, but the site classifies eight value
shapes (`Hash`, `Array`, `Seq`, `Slip`, `LazyList`, `Set`/`Bag`/`Mix`, scalar,
`Nil`/`Package`) and each arm has to be re-checked against a celled input, plus
the same question asked of the other `is <Type>` consumers
(`register_var_container_type_metadata`, `tag_container_metadata`,
`attr_build_defaults.rs`). Worth doing deliberately with the QuantHash roast
files as the gate, not as a rider on another change.

## Acceptance

The repro prints `3` for `my Int @a`, `my Str %h`, and `my %h is BagHash`;
`roast/S02-types/{baghash,mixhash,bag,set,mix}.t` still pass; the
`ApplyVarTrait` subtraction in `CompiledCode::compute_free_vars` can be deleted;
and `t/container-capture-cell-dichotomy.t` test 13 is rewritten from "a typed
container is left unboxed" to the binding assertion the rest of the file makes.
