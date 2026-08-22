# `MixHash (^) MixHash` / `MixHash (+) MixHash` (symmetric-difference / union) produce garbage output; the same ops work fine on plain `Mix`

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/MixHash.rakudoc:99`).
The harness bucketed this whole block as `raku-drift-from-doc` (the doc's stated element order
for one line doesn't match current raku's output), but a genuine, separate bug hides underneath
that drift: re-verified directly against current `raku`, isolating the divergence to the
weighted set-operators on `MixHash` specifically.

## Root cause hypothesis

The Baggy/Mix set operators (`(^)` symmetric difference, `(+)`/`(<+>)` union addition, etc.)
work correctly on immutable `Mix` operands:

```raku
my $a = (2 => 2, 4).Mix;
my $b = (2 => 1.5, 3 => 2, 4).Mix;
say $a (^) $b;   # Mix(2(0.5) 3(2))          -- matches raku exactly
say $a (+) $b;   # Mix(2(3.5) 3(2) 4(2))     -- matches raku exactly
```

But the same operators on **mutable `MixHash`** operands produce clearly wrong results — not
just a different element order, but the wrong *shape* entirely (raw `key => value` `Pair`
gists instead of computed `key(weight)` results, and the union/symmetric-difference math is not
actually performed — mutsu just seems to dump one or both operand's raw pairs):

```raku
my ($a, $b) = MixHash(2 => 2, 4), MixHash(2 => 1.5, 3 => 2, 4);
say $a (^) $b;   # raku: MixHash(2(0.5) 3(2))
                 # mutsu: MixHash(2 => 2 2 => 1.5 3 => 2)          -- wrong
say $a (+) $b;   # raku: MixHash(2(3.5) 3(2) 4(2))
                 # mutsu: MixHash(4(2) 2 => 2 2 => 1.5 3 => 2)     -- wrong
```

This strongly suggests `MixHash` operands take a different (and broken/unfinished) dispatch
path for these weighted set operators than plain `Mix` does — likely falling through to a
generic Hash-merge or pair-concatenation fallback instead of the same weighted symmetric-
difference/union arithmetic that `Mix` correctly uses.

## Minimal repro

```raku
my ($a, $b) = MixHash(2 => 2, 4), MixHash(2 => 1.5, 3 => 2, 4);
say $a (^) $b;
say $a (+) $b;
```

- `raku`: `MixHash(2(0.5) 3(2))` then `MixHash(2(3.5) 3(2) 4(2))` (element order may vary, but
  the weights/keys are always these).
- `mutsu` (`target/debug/mutsu`): `MixHash(2 => 2 2 => 1.5 3 => 2)` then
  `MixHash(4(2) 2 => 2 2 => 1.5 3 => 2)` — wrong keys/weights, `=>` pair-gist syntax mixed in
  with weight-gist syntax.

## Affected files (starting point)

- Set-operator (`(^)`/`(+)`/`(&)`/etc.) dispatch for `Mix`/`MixHash`/`Bag`/`BagHash` operands —
  likely `src/vm/vm_set_ops.rs` — needs to route `MixHash` operands through the same weighted
  computation `Mix` already uses correctly, rather than falling back to a generic/incomplete
  path for the mutable variant.
