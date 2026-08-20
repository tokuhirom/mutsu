# Array/Hash elements are stored bare — element reads lack itemization (store-side residue)

## Status

**Designed. The mechanism decision now lives in
[ADR-0040](../../docs/adr/0040-array-hash-elements-are-itemized-at-the-store.md)** (2026-08-20):
itemize at the element *store* via the existing `Value::item()`, applied after each store site's own
flattening decision, for real mutable `Array`/`Hash` only. Read ADR-0040 before starting — it carries
the 25-row divergence matrix re-measured on `main` (52631889f), the store-site inventory, the
phasing, and the open questions. This file stays open only as the tracking record; retire it to
`news/2026-08/` when ADR-0040's slice 5 lands.

The **bind-side half was already fixed** (2026-08-11, `news/2026-08/param-bind-itemization.md`): a
value bound to a plain `$`-sigiled parameter — for-loop pointy params, sub/closure positional and
named params, map/grep block params, placeholders — is itemized, matching raku's signature binder.
Pinned by `t/param-bind-itemization.t`.

## What remains

raku's model is that array/hash *elements are Scalar containers*, so an element read is itemized
even with no parameter binding involved. mutsu stores elements bare:

```
$ target/debug/mutsu -e 'my @c = [<a b>],[<c d>]; say @c[0].raku'   # ["a", "b"]   raku: $["a", "b"]
$ target/debug/mutsu -e 'my %h = a => [1,2]; say %h<a>.raku'        # [1, 2]       raku: $[1, 2]
$ target/debug/mutsu -e 'my @c = [<a b>],; for @c { say .raku }'    # ["a", "b"]   raku: $["a", "b"]
```

`my $v = @c[0]` DOES itemize (scalar assignment goes through `itemize_scalar_store`), which is why
the gap looks narrower than it is. ADR-0040 §1.3 has the full matrix: 24 of 25 probes diverge,
covering direct element reads, slices, `.head`/`.tail`/`.first`/`.sort`/`.reverse`/`.map`/`.pairs`/
`.kv`/`.Slip`, implicit-topic iteration, hash-value reads, element assign, `push`, `append`, and
autovivification.

## Two corrections to this file's original framing

- **The cost estimate was too high.** This file sized the store-side half as "a survey-sized campaign
  with its own fallout class ... changes what is IN every array". Measured (ADR-0040 §1.4): writing
  the post-fix state by hand — `my @i = $[1,2], $[3,4]` — and comparing it against today's bare
  elements across 25 behavioural probes gives **25 identical results**, and ten renderer/equality
  cross-checks match raku exactly. The itemization flag rides on the same shared `Gc<ArrayData>` and
  every flattening decision point already consults `is_itemized()`. What is genuinely survey-sized is
  only the enumeration of store sites, not the consequences of storing itemized values.

- **The "list-destructuring bind write-through" bullet is misfiled and is NOT part of this.**
  `my (\a, \b) := ($x, $y); a = 10` does not propagate to `$x` because the desugar builds
  `my @__destructure_tmp__ = [$x, $y].list` and reads `@__destructure_tmp__[i]` — the temp array
  holds *copies*, so no element containerization anywhere could reach `$x`. The single-variable form
  (`my \a := $x`) already works. The fix is in the desugar (emit N single binds); see ADR-0040 §1.7.
  Its failure mode has also changed since this file was written: it now dies with
  `Cannot assign to a readonly variable (a) or a value` rather than silently no-opping.

## Verification once fixed

```
$ mutsu -e 'my @c = [<a b>],[<c d>]; my @d = @c; say @d[0].raku'   # $["a", "b"]
$ mutsu -e 'my @c = [<a b>],; for @c { say .raku }'                # $["a", "b"]
$ mutsu -e 'my %h = a => [1,2]; say %h<a>.raku'                    # $[1, 2]
$ mutsu -e 'my @c = [<a b>],; sub t(*@a){@a.elems}; say t(@c[0])'  # 1
$ mutsu -e 'my @c = [<a b>],[<c d>]; say @c.raku'                  # [["a", "b"], ["c", "d"]] (unchanged)
```
