# A module's file-scope `my $x = [...]` still collides with the caller's `my $x`

Found while building the ADR-0039 slice-2 acceptance matrix
(`t/container-lexical-slot-resolution.t`, 2026-09-06). It is a **scalar** lane
divergence, not a container one, and it reproduces identically with and without
slice 2 — so it is out of ADR-0039's scope and tracked separately here.

## Repro

`t/lib/ContainerSlotLexical.rakumod` declares, at file scope:

```raku
unit module ContainerSlotLexical;
my $anon = [<a b>];
sub anon-push($v) is export { $anon.push($v) }
sub anon-peek()   is export { $anon.join(",") }
```

and a consumer:

```raku
use ContainerSlotLexical;
my $anon = [<x y z>];
anon-push("c");
say anon-peek();        # raku: a,b,c   mutsu: a,b
say $anon.join(",");    # raku: x,y,z   mutsu: x,y,z,c
```

The module's push lands on the **consumer's** array: the module routine
resolved `$anon` to the loading scope's binding.

## Why it is not "just the ADR-0039 bug again"

The sigil is `$`. The value is an Array, but the *binding* is a scalar, so it
takes the scalar unit-lexical lane (ADR-0024), not the `@`/`%` container lane
ADR-0039 slice 1 gave a cell store and slice 2 slot-addressed. The container
shapes (`my @items`, `my %hs`, `our @a`, `state @a`, `my Int @a`) are all
correct today — see `t/container-lexical-slot-resolution.t`.

## The part that makes it hard to reduce

The divergence is **context-sensitive**, which is the real finding here. The
two-line repro above passes on its own; it only fails once the consumer file
also declares several *other* mainline lexicals and named subs before it (the
shape it was found in is `t/container-lexical-slot-resolution.t` with the
module's `@items`/`@ours`/`@st`/`@ti` consumers, `sub dyn-wrapper`, and the
`add-name`/`read-names` family declared afterwards). So the trigger is not the
`$anon` declaration itself but *which* names the mainline capture
(`exec_register_sub_op` → `unit_lexicals[MAINLINE_UNIT_KEY]`) ends up holding
by the time the module routine runs.

`unit_lexical_container_cell` (`src/vm/vm_env_helpers.rs`) probes
`unit_lexicals[MAINLINE_UNIT_KEY]` **before** `unit_lexical_slot(name)`, i.e.
the *loading script's* mainline lexicals win over the running routine's own
compunit. That precedence is the first thing to check; `unit_lexical_slot`'s
own ordering is the second.

## Suggested first steps

1. Bisect the consumer prefix mechanically (delete one preceding declaration at
   a time from `t/container-lexical-slot-resolution.t` until the `$anon` rows
   pass) to name the exact trigger, rather than guessing.
2. Then decide whether the fix is a precedence change in
   `unit_lexical_container_cell` / `unit_lexical_slot` or a capture-set fix in
   `exec_register_sub_op`.
3. When fixed, restore the two rows that were removed from
   `t/container-lexical-slot-resolution.t` (they are marked with a comment
   pointing at this file) and bump its plan from 52 to 54.
