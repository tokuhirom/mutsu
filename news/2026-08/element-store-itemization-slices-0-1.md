# Array/Hash elements itemize at the mutation-site store (ADR-0040 slices 0-1)

Raku's model is that every `Array`/`Hash` element *is* a `Scalar` container: reading it back gives
one item in list context, and it renders itemized (`$["a", "b"]`, not `["a", "b"]`). mutsu already
itemized values bound to a plain `$`-sigiled parameter (`news/2026-08/param-bind-itemization.md`),
but stored array/hash elements themselves stayed bare, so `my @a; @a.push([7,8]); say @a[0].raku`
printed `[7, 8]` instead of raku's `$[7, 8]`.

[ADR-0040](../../docs/adr/0040-array-hash-elements-are-itemized-at-the-store.md) decided to fix this
at the element *store* rather than compensating in readers — itemizing an aggregate value the
instant it is written into a real `Array`/`Hash` element, using the existing `Value::item()`
itemization primitive (or its narrower, allocation-free `ValueView`-preserving sibling
`Interpreter::itemize_value`, used at the majority of sites). Because the itemization flag rides on
the same shared backing (`Gc<ArrayData>` / `Gc<HashData>`), and every list-context flattening
decision point already consults it, a value itemized once at the store stays itemized through
`map`/`grep`/`sort`/`head`/`pairs`/slices/the implicit topic with no changes needed to any of those
producers.

Slices 0-1 land the full set of mutation sites: element assign (`@a[i] = v`, `%h<k> = v`), single-
and nested-level autovivification (`@a[5][0] = 1` now itemizes the freshly-created `@a[5]`;
`%h<a><b> = 1` likewise), and `push`/`unshift`/`append`/`prepend`/`splice`, always applied *after*
each site's own one-arg-rule/Slip-flattening decision so arity never changes — `push(1,2)` still
adds two bare elements, `push([1,2])` still adds one itemized element.

Reference-shared push (`@a.push(@b)`, which shares a live `ContainerRef` cell with the source
variable) needed its own representation choice: the shared cell's own content stays untouched (so
`@b` read directly still renders bare), and only the *pushed element's own* wrapper carries the
itemization — `@a[0].raku` is `$[1, 2]` while `@b.raku` stays `[1, 2]`, matching raku exactly, and a
later `@b.push(3)` still propagates through the shared cell to `@a[0]`. This introduced a new
`Scalar(ContainerRef(_))` value shape that method dispatch's decontainerize step did not know about,
fixed by widening it to see through that shape for every method except the renderers
(`raku`/`gist`/`perl`), which need the wrapper intact to render the `$`.

A pre-existing, unrelated `splice` arity bug was found and filed separately rather than fixed here
(`todo/tickets/splice-multi-arg-array-incorrectly-flattens.md`) to keep this change's blast radius
scoped to itemization.

The acceptance oracle, `t/element-store-itemization.t`, pins the full ADR-0040 §1.3 divergence
matrix (dual-oracled against `raku`) plus the invariants that must never move — an `Array`'s own
`.raku` still de-itemizes its elements, `Pair`/`Set`/`Int` elements stay unwrapped, and the arity
rules are unaffected. Rows that depend on itemizing at *construction* time (list-assign, literal
construction) or on `.VAR` reflection stay `todo`-marked for ADR-0040 slices 2-3, which are follow-up
work.
