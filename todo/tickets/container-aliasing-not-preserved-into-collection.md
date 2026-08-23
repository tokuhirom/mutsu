# A scalar's container isn't aliased when pushed/assigned into a collection without `.item`/`.clone` — mutsu snapshots by value where raku aliases

Discovered via the doc-diff harness on two files, both showing the same shape of divergence:
`raku-doc/doc/Language/traps.rakudoc` (around line 406) and
`raku-doc/doc/Language/variables.rakudoc` (around line 853). Filed as one ticket since both
repros demonstrate the identical underlying behavior.

## Repro 1 (traps.rakudoc:406)

```
my @arr;
my ($a, $b) = (1,1);
for ^5 {
    ($a,$b) = ($b, $a+$b);
    @arr.push: ($a.item, $b.item);
    say @arr
};
```

- raku: every previously-pushed entry's displayed value **changes** on each iteration
  (`[(1 2)]` → `[(2 3) (2 3)]` → `[(3 5) (3 5) (3 5)]` → ...) — i.e. `.item`-ing `$a`/`$b` here
  still leaves every pushed tuple aliased to the *same* `$a`/`$b` containers, so they all show
  the latest value (this is presented by the doc as a "trap" / surprising gotcha)
- mutsu: each entry keeps the value it had *at push time* (`[(1 2)]` → `[(1 2) (2 3)]` → `[(1 2)
  (2 3) (3 5)]` → ...) — i.e. mutsu's push makes an independent snapshot, so it does NOT
  reproduce the aliasing trap

## Repro 2 (variables.rakudoc:853)

```
my @a;
my @a-cloned;
sub f() {
    state $i;
    $i++;
    @a       .push: "k$i" => $i;
    @a-cloned.push: "k$i" => $i.clone;
};
f for 1..3;
say @a;         # raku: [k1 => 3 k2 => 3 k3 => 3]   mutsu: [k1 => 1 k2 => 2 k3 => 3]
say @a-cloned;  # raku & mutsu agree: [k1 => 1 k2 => 2 k3 => 3]
```

Same shape: pushing a `Pair` whose value is the *un*-cloned `state $i` scalar should keep every
pushed entry aliased to the single shared `$i` container (so they all end up showing the final
value, `3`), while `.clone`-ing breaks the alias and gives the expected independent snapshots
(both agree on the cloned case). mutsu behaves as if `.clone` were always implied — the uncloned
case gives the same (non-aliased) result as the cloned case.

## Root cause guess

mutsu's array `.push`/collection-store path (and/or Pair-value construction) always copies a
scalar's *value* into the new element slot rather than preserving a live reference to the
original container when the source expression is a bare variable or `.item`-ized scalar without
an explicit `.clone`. Per raku's actual (if gotcha-prone) semantics, `.item` on a container still
shares the *same* underlying container — it only prevents auto-flattening in list context, it
does not imply a copy. This may be the same store-side itemization/container question already
being tracked architecturally (see `docs/adr/0040-array-hash-elements-are-itemized-at-the-store.md`
and its follow-up work, e.g. `news/2026-08/splice-replacement-arg-one-arg-rule.md`'s
neighborhood) — worth checking against that ADR before implementing, since "does push alias or
copy a container" is a core container-semantics question that ADR's itemization work may already
touch.

## Affected files (starting point)

- `src/runtime/methods_mut_dispatch.rs` — `.push`/array-store implementation
- `src/value/` — scalar container aliasing vs. value-copy semantics on collection insertion

## Suggested next step

Read `docs/adr/0040-array-hash-elements-are-itemized-at-the-store.md` first to see whether this
aliasing gap is already in that ADR's scope or is a distinct, pre-existing bug it doesn't cover.
