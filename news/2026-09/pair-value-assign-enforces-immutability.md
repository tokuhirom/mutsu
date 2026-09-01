# `Pair.value = X` now raises `X::Assignment::RO` when the value has no container

Raku's `Pair.value` is an `rw` accessor onto whatever the Pair holds, so
assigning through it is only legal when the Pair's value is itself a container.
A Pair built from a literal holds the bare value and the assignment dies:

```
$ raku -e 'my $p = (1 => "a"); $p.value = "z"'
Cannot modify an immutable Str (a)
```

mutsu accepted the assignment and mutated the Pair in place (`1 => z`). It was
not specific to a bare scalar — the same gap showed through an array element
(`@t[0].value = "z"`) and through a `for` loop's topic
(`.value = "z" for @t`) — and it was also **ADR-0036 §1.3 row 11**
(`my $l = (1, 2); $l.pairs[0].value = 3` must die, and silently succeeded as a
no-op). All of these now match raku verbatim, message included.

The mirror case is what made this a missing *check* rather than a missing
feature: `my $v = 1; my $p = (1 => $v); $p.value = 2` must succeed and write
through to `$v`, and it always did.

## What was actually swallowing the write

Two mechanisms, and the ticket's own root cause — inherited from ADR-0036 —
named only one of them:

1. **The standalone-pair env rebind.** With no backing container found, the
   `.value` lvalue path scanned `env` for any binding holding a `Pair` with the
   same key and old value and rebound those to a fresh `Pair`, faking an alias
   raku does not have. An `X::Assignment::RO` guard already sat immediately
   above it with the right shape — it just only fired for a `Bool`
   (`Set.pairs[0].value = 0`). Generalising that guard to every immutable
   scalar leaf (`Int`, `BigInt`, `Num`, `Str`, `Bool`, `Rat`, `FatRat`,
   `BigRat`, `Complex`, `Enum`, `Nil`, a type object) closes it.

   The guard deliberately does **not** fire for a reference value
   (`Array`/`Hash`/`Instance`/`Proxy`/…): those are mutable in place and their
   writes must keep working. That is not a hedge — the one place in the whole
   roast whitelist that still reaches the rebinds is `S02-types/pair.t`'s
   `(%(<a b c d>) => %(<e f g h>)).invert`, whose pair value is a `Hash`.

2. **The array env-scan, which ADR-0036 had measured as "never fires here".**
   It does — just not from the shape the ADR instrumented. At the top level
   `$l` is a `ContainerRef`, so scanning `env` for an `Array` binding misses it
   and the guard is reached. Inside a closure (`dies-ok { … }`, which is how
   row 11 is actually written) the binding *is* a plain `Array`, the scan finds
   `$l`'s own list, rebuilds it, and reports success — so row 11 passed at the
   command line and failed in its own test file. The scan now skips
   `ArrayKind::List` / `ItemList`, which are raku's immutable list types:
   `my $l = (1, 2); $l.pairs[0].value = 3` dies, while `my $l = [1, 2]` and
   `my @a` keep writing through, exactly as raku does. This is the third
   consecutive wrong root cause recorded for this code path; measuring one
   shape is not measuring the mechanism.

## Prerequisites, and the tests that encoded the bug

Both prerequisites had landed the same day, each the same one-flag fix —
capture with `box_type_objects` set, so an *uninitialized* declared scalar (a
bare type object, but still a container) is boxed into its own cell: the
fat-arrow form in `MakePair`/`MakeNamedArg`, and the `Pair.new("k", $x)` form
in `exec_call_method_mut`'s native-Pair unbox. Without them, deleting the fake
would have turned a working alias into a hard error.

Four `t/` files pinned the compensator rather than the spec, and were corrected
against raku (which dies on every one of them): `t/pair-value-container.t`'s
literal-Pair row (it was already marked DIVERGES) and its new array-element and
loop-topic rows; `t/lvalue-method-writeback-coherence.t`'s
`my $p = a => 5; $p.value--` and `my $q = x => 0; $q.value = 1` blocks;
`t/for-pairs-value-quanthash-writeback.t`'s `.value--` parser probe; and
`t/subscript-pair-element-container.t`, where row 11 loses its `todo` marker
and row 9 gains a `try` (it still diverges — `.pairs` routing is deferred — but
now diverges by dying rather than by silence).

## Known residue

`my $l = [1, 2]; $l.pairs[0].value = 3` writes through in raku (`[3 2]`); in
mutsu it was a silent no-op before and raises `X::Assignment::RO` now. The
scalar-held `Array` is behind a `ContainerRef`, so the env-scan does not see it
— the same blindness that made the top-level `List` case reach the guard. The
fix is not to widen the compensator (ADR-0036's direction is to delete it) but
to route `.pairs` through the container-aware producer, which is
`todo/deep/pairs-element-containers-leak-through-pair-value-consumers.md`.

Separately, correcting the coherence test to the container form exposed a
pre-existing hang: `$p.value++` on a container-backed Pair stops accumulating
on the second loop iteration and overflows the stack on the third. Filed as
`todo/tickets/container-pair-value-increment-in-loop-stalls-then-hangs.md`.
