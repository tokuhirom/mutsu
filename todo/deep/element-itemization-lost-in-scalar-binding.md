# Array/Hash elements are stored bare — element reads lack itemization (store-side residue)

## Status

The **bind-side half of this ticket is fixed** (2026-08-11,
`news/2026-08/param-bind-itemization.md`): a value bound to a plain
`$`-sigiled parameter — for-loop params (single and multi), sub/closure
positional and named params, map/grep block params, placeholders — is now
itemized, matching raku's signature binder. That fixed the original symptom
(`CSV::Table` `t/5-save.t`'s sprintf explosion; the suite is 10/10) and the
`for @c -> $v { $v.raku }` divergence. Pinned by
`t/param-bind-itemization.t`.

What REMAINS is the store-side half: raku's model is that array/hash
*elements are Scalar containers*, so an element read is itemized even with no
parameter binding involved. mutsu stores elements bare:

```
$ target/debug/mutsu -e 'my @c = [<a b>],[<c d>]; my @d = @c; say @d[0].raku; for @c { say .raku }'
["a", "b"]        # raku: $["a", "b"]
["a", "b"] ...    # raku: $["a", "b"] (implicit topic binds the element CONTAINER)
```

(`my $v = @c[0]` DOES itemize — scalar assignment goes through
`itemize_scalar_store` — so the gap is direct element reads: `@d[0].raku`,
slices `@c[0,1]`, implicit-topic iteration, `.head`/`.tail`/`.first`/
`.sort`/`.reverse` results, hash-value reads `%h<a>`.)

## Why this is deep, not a ticket

Fixing it read-side would mean touching every element-read site (indexing,
slices, dozens of list methods, iterators) — each must know its source is a
real Array/Hash, which `.kv`-through-Seq loses. Fixing it store-side (itemize
at element *storage*: list-assign into `@`, push/unshift/splice, element
assign, `[...]` construction) is the raku-faithful single model but changes
what is IN every array — a survey-sized campaign with its own fallout class
(the bind-side campaign hit two consumers: `.cache` identity-return and
`&combinations`; store-side will hit more).

Note on Track B: ADR-0001's "element `ContainerRef` cells fused with the GC
campaign" framing is HISTORY, not a live constraint — the GC (layer 3a
cycle collector), NaN-boxing (3b), and JIT (layer 4) all shipped and are
default on (ADR-0001 §7, 2026-08-02), and the "do not start Track B
standalone" rule was superseded by ADR-0013 §7 (the `GcBox`/`UnsafeCell`
interior-mutability refinement made the `gc_contents_mut` sites sound
without Value-layer element cells). So there is no pending campaign to fold
this into: run the store-side itemization as its own measured campaign —
just not as a drive-by.

## Affected

- `.raku`/`.gist` of arrays-of-arrays read back element-wise (`$[...]` vs
  `[...]` — the `.raku` residues family, PLAN §8 QA).
- Implicit-topic iteration over `@`-arrays whose body relies on the element
  being ONE item in list context (the sprintf shape, now only reachable via
  `for @c { ... $_ ... }` — the `-> $v` form is fixed).
- List-destructuring bind write-through: `my (\a, \b) := my ($x, $y); a = 10;`
  (or the sigilled `my ($a, $b) := ($x, $y);`) never propagates to `$x`,
  because the destructuring desugar reads each target's RHS out of a temp
  array by index (`Expr::Index { target: ArrayVar("__destructure_tmp__"),
  index: i }`), which has no per-element container to alias. Found triaging
  `Math::Interval`'s `TWEAK` (`todo/tickets/dist-test-suite-failures-batch.md`);
  the single-variable case (`my \a := $x`) was fixed separately in
  `news/2026-08/sigilless-bind-writable-alias.md`, but the list-bind form
  needs this store-side fix.

## Verification once fixed

```
$ mutsu -e 'my @c = [<a b>],[<c d>]; my @d = @c; say @d[0].raku'   # $["a", "b"]
$ mutsu -e 'my @c = [<a b>],; for @c { say .raku }'                # $["a", "b"]
$ mutsu -e 'my %h = a => [1,2]; say %h<a>.raku'                    # $[1, 2]
```
