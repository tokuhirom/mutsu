# Array and Hash elements are itemized at construction too (ADR-0040 slice 2)

Raku's model is that every `Array`/`Hash` element **is** a `Scalar` container, so an element
handed out is one item and renders itemized. [ADR-0040](../../docs/adr/0040-array-hash-elements-are-itemized-at-the-store.md)
decided to put that property at the element *store* rather than compensate for it at the read.
Slice 1 covered the *mutation* sites (element assign, autovivification,
`push`/`unshift`/`append`/`prepend`/`splice`). **Slice 2 covers the construction sites** —
list-assign into `@a`/`%h`, real-container literal construction, `.Array`/`.Hash` coercion, and
JSON decoding — which turns rows 01-18 and 23 of the ADR's 25-row divergence matrix green. Only
row 24 (`.VAR` reflection) is left; that is slice 3.

```
$ mutsu -e 'my @c = [<a b>],[<c d>]; say @c[0].raku'
$["a", "b"]                          # was ["a", "b"]
$ mutsu -e 'my %h = a => [1,2]; say %h<a>.raku'
$[1, 2]                              # was [1, 2]
$ mutsu -e 'sub takes(*@a){@a.elems}; my @c = [1,2],[3,4]; say takes(@c[0])'
1                                    # was 2
$ mutsu -e 'my @c = [1,2],[3,4]; say @c.raku'
[[1, 2], [3, 4]]                     # unchanged — an Array's OWN .raku de-itemizes
```

**This is the most user-visible change in the campaign**: any nested data structure printed
element-wise now gains a `$` in `.raku`. That is what raku prints — `.raku` is reporting that the
value sits in a container — but it is worth knowing about when reading diffs of `.raku` output.
`.gist`, `.Str` and `to-json` are unaffected.

## Why the store, and why it propagates for free

The decisive property is that itemization *rides along on the copied `Value`*. Because the flag
is set once when a value enters a real container, every downstream element producer — `[i]`,
slices, `.head`/`.tail`/`.first`, `map`/`grep`/`sort`/`reverse`, `.pairs`/`.kv`, the implicit
`for` topic — inherits it with no per-method work. A read-side fix would have had to be
re-derived at each of those, and re-opened by the next one added.

The hooks themselves are small: a shared `itemize_real_array_elements` applied at
`coerce_to_array`'s tail, at the two `@`-assign entry points (under a `!is_bind` guard — a bound
`List`'s elements are *not* containers), at the array-literal ops, and at `.Array`; plus a
`hash_stored_value` helper at every `map.insert` value site in the hash builders and the `%(…)`
literal ops.

## The perf question resolved itself

ADR-0040 §5.2 flagged one real risk: `coerce_to_array` shares the backing `Gc` for `my @a = @b`,
and a per-element hook could force a full rebuild. The hook scans first and only rebuilds on a
hit — and, crucially, slice 2 makes itemization **idempotent along a copy chain**: `@b`'s elements
were already itemized when `@b` was built, so `my @a = @b` finds nothing to do and keeps sharing
the `Gc`. The rebuild is paid at most once per aggregate, at the moment it first enters a real
container, which is the literal/list-assign site that was already doing a per-element `match`.

## The work was the counter-currents, not the hooks

Twelve sites asked a question *about a value* while holding something itemized *because it is an
element* — the slice-2 recurrence of slice 1's `value_to_list_for_receiver` discovery. Six were
caught by the local `t/` suite, two more by a deliberate sweep of serializers and
receiver-decomposing methods, and the last four only by the targeted roast sweep — each in a
different subsystem. Each needed the same distinction drawn explicitly:

- **`.antipairs` / `.invert`** — Rakudo builds these with `Pair.antipair`, which *reads* `$!value`;
  an attribute read decontainerizes. So the same element is itemized as a pair's value
  (`@c.pairs.raku` is `(0 => $[1, 2],)`) and bare as a pair's key (`@c.antipairs.raku` is
  `([1, 2] => 0,)`).
- **`.raku` through a `:=`-bound element** — the "an Array's own `.raku` de-itemizes" rule had to
  see through ADR-0036's element cell, or a bound element and its un-bound sibling disagreed.
- **`deepmap`/`nodemap`/`duckmap`** — the leaf-vs-descend test is about what the value *is*, so an
  itemized `Range` element must still descend.
- **Destructuring sub-signatures** — binding an element to an `@`/`%` parameter reads the
  element's value. `Digest::RIPEMD`'s `-> [&f, $r, @K, $s]` broke without this.
- **`splice` on an element receiver** — an itemized array is still a real array *as a receiver*;
  `@w[0].splice(*-2, 1)` has to resolve `*-2` against its own length.
- **The reduce metaop** — `[Z] @m` reads the element *values* and zips them, while the explicit
  `@m[0] Z @m[1]` receives the elements themselves and does not. Both behaviours are raku's;
  they simply are not the same expression.
- **`.Array`** — builds a *new* Array, which is not an element of anything, so the receiver's own
  itemization is dropped (the neighbouring `.list` arm already did this).
- **Set-operator membership** — the container is the receiver of the test, so it decomposes;
  the *needle* is deliberately not touched, because a `Set`'s members keep their itemization in
  raku (`Set.new($[1, 2])`) and `.WHICH` membership has to see what was stored.
- **`.Map`** — a `Map`'s values are not containers, and the existing decont there only unwrapped
  a `Scalar`, missing the kind/flag form. This is what made `Foo.new(|%args.Map)` bind an `@.a`
  attribute to one itemized array instead of three elements.
- **`is-deeply`** — it normalizes a `Seq` to a `List` before comparing, and had to see through
  the wrapper to find the Seq (`eqv` itself was already right).
- **`.toggle` and `<>`** — `.toggle` decomposes its own receiver; `<>` already cleared an
  itemized `ArrayKind` but not the Hash itemization flag.

One desugar also became visibly wrong: `my (@a, @b) := (@x, @y)` staged the RHS in a real
`Array` and then *assigned* each element to its target. `my @a = $[1, 2]` is `[[1, 2],]` — correct
for `=`, wrong for `:=`. The per-target declaration is now a genuine bind in binding mode, which
decontainerizes the staged element the way a real bind does; `=`-mode keeps its greedy-slurp
semantics unchanged.

And one gap turned out to point the *other* way: mutsu's native JSON decoder built its `Hash`es
and `Array`es directly, so it bypassed every hook and was *missing* the itemization
(`from-json('{"a":[1,2]}')<a>.raku` was `[1, 2]`, raku's is `$[1, 2]`). Fixed at
`Parser::finish_object`/`finish_array`; the `:immutable` forms decode to `Map`/`List`, whose
elements are not containers, and are deliberately left alone.

## Verification

`t/element-store-itemization.t` grew from 46 to 100 assertions — every new one dual-oracled
against `raku` — covering the construction sites, all eight counter-currents, the `:=`-bind and
`(...)`-List-literal invariants, the arity invariants, and native-array safety. ADR-0040 §5's
open question 4 ("does anything depend on an element being bare?") was answered no by 60
dual-oracled probes over `to-json`/`from-json`, `is-deeply`, `eqv`, `.WHICH`, `.Str`/`.gist`/
`.join`, parameter binding, hyper method calls, and the whole receiver-decomposition method
family.

Closed by this slice: `todo/tickets/array-literal-nested-element-itemization-lost-in-raku.md`
(`say .raku for [3,2,[1,0]]` now prints `$[1, 0]`).
