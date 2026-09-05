# `%h<self> = %h` deep-copies instead of aliasing, and leaks a marker into the renderers

Assigning a hash into one of its own keys does not build a cycle in mutsu. It
stores a *copy*, and the copy carries an internal `__mutsu_self_hash_ref` marker
that leaks straight into both renderers:

```raku
my %h;
%h<a> = 1;
%h<self> = %h;

say %h.gist;
# raku:  (\Hash_3371917301808 = {a => 1, self => Hash_3371917301808})
# mutsu: {a => 1, self => {a => 1, self => __mutsu_self_hash_ref => True}}

say %h.raku;
# raku:  ((my %Hash_3140962142352) = {:a(1), :self(%Hash_3140962142352)})
# mutsu: {:a(1), :self({:a(1), :self(:__mutsu_self_hash_ref)})}
```

Measured 2026-09-05 on `main` at `37dd63f33`, plus the branch that closed
`news/2026-09/gist-of-a-circular-container-renders-a-back-reference.md`.

## Why this is not the gist-cycle bug

It looks like a rendering bug and is not one. The gist/`.raku` cycle rule is
now correct and is exercised on hashes: a cycle that reaches a hash *through*
an array renders exactly as rakudo does, because that hash node is genuinely
shared —

```raku
my @m; my %n; @m = 1, %n; %n<x> = @m;
say @m.gist;   # (\Array_… = [1 {x => Array_…}])   -- matches raku
```

The direct `%h<self> = %h` case never reaches the cycle detector at all, because
by the time the renderer walks it there is no cycle: the `Gc<HashData>` in the
`self` slot is a different node from `%h`'s own. The bug is on the **store**
side.

## Root cause (hypothesis — verify before designing)

Two things are entangled and both need measuring:

1. **The self-assignment is snapshotted.** `%h<self> = %h` appears to clone the
   hash rather than storing a reference to the same `Gc<HashData>`. rakudo
   stores the container, so the structure is genuinely circular. Note that the
   *array* spelling (`@c = 42, @c`) does build a real cycle, so this asymmetry
   is specific to the hash store path — start by diffing the two.
2. **`__mutsu_self_hash_ref` is a sentinel that escapes.** Grep shows it is an
   internal marker; whatever writes it intends it to be recognised on the way
   out, and neither `gist_value` (`src/runtime/utils/gist.rs`) nor `raku_value`
   (`src/builtins/methods_0arg/raku_repr.rs`) strips it. Even if (1) is fixed,
   the marker's remaining producers/consumers need auditing — a sentinel that
   reaches a renderer is the same class of bug as the `Nil`-as-hole collision
   ADR-0049 retired.

## Why it is a ticket rather than a one-liner

The fix is a store-path change (`%h<k> = %h` must install the same node), which
touches hash element assignment — the path ADR-0040 slice 4 and ADR-0013's
interior-mutability work both run through — and it must not regress the ordinary
`%a = %b` copy semantics, which genuinely *do* copy. Deciding which spellings
alias and which copy is the actual work; the renderer side is downstream of it.

Check the `Array`-into-`Hash` and `Hash`-into-`Array` mixed spellings at the
same time, and re-run the `%h<self>` rows of any shape matrix afterwards.

## Reproduce

The two `say` lines above, no fixtures. `t/gist-circular-container.t` covers the
shapes that already work and deliberately does not cover this one.
