# The immutable-topic receiver oracle learns the shapes raku actually refuses

`todo/deep/immutable-lvalues-that-mutsu-still-lets-you-assign-to.md` section A
collected the receivers where mutsu silently accepted `$_ = ...` on a topic
rakudo refuses. The file's own conclusion was that section A "cannot be closed
by a local runtime test" and had to wait for either section B or a grown
compile-time receiver oracle. Re-measuring the whole survey on 2026-09-07
showed most of section A is the *second* route, and that route is a lot smaller
than the file assumed: four of its six rows, plus every row of two families the
survey had not enumerated, are decided by `Compiler::for_iterable_yields_bare_items`
alone.

## The rule, measured

raku binds a `for` loop's implicit topic — and a `.map`/`.grep`/`.first`
callback's `$_` — to the source ELEMENT. `$_ = ...` is legal exactly when that
element has a `Scalar` behind it. Measured against rakudo, that makes the
verdict a property of the receiver's syntax in three families the oracle did not
know:

| receiver | rakudo | mutsu before |
|---|---|---|
| `for %h { $_ = 5 }`, `%h.map({ $_ = 5 })`, `%h.grep(...)` | throws | silently dropped |
| `for @a.List`, `@a.List.map`, `@a.pairs`, `@a.antipairs`, `%h.kv`, `%h.List` | throws | silently dropped |
| `for (@a,)`, `for @a, @b`, `for $x, @a`, `for $x, 2`, `for ($x + 1,)`, `for (1..2), (3..4)` | throws | silently dropped, or wrote one item and continued |
| `(1, 2).first({ $_ = 5 })` | throws | answered `1` |

and — the half that keeps the change honest — leaves these writable, because
each one hands out the source's own containers:

`for @a`, `for (@a)`, `for @a.list`, `for @a.values`, `for @a.Seq`,
`for %h.values`, `for ($x, $y)`, `($x, $y).map({ $_ = 5 })`, `for ($x,)`,
`@a.first({ $_ = 5 })`, `@a.values.first({ $_ = 5 })`.

A **list literal is element-wise**: `($x, $y)` is a `List` of the two `Scalar`s
and writes through, while any item with no `Scalar` behind it — a bare value, an
`@`/`%` variable, a `Range`, an arithmetic result — makes the topic immutable.
That is why the oracle's `ArrayLiteral` arm flipped from "every item is a
literal" to "**any** item is provably bare": the mark is one per loop rather
than per item, and raku throws at the bare item either way.

## The marking had to be split shallow/deep

`for %h { $_ = 5 }` throws in raku but `for %h { .value = 5 }` writes the hash,
and so do `for %h -> $p { $p.value = 7 }` and `for %h.pairs { .value = 9 }`. The
survey predicted this correctly: the `for` loop had fused its topic mark with
the `__mutsu_deep_readonly::_` env flag that also refuses a method lvalue, so
extending the oracle to `%h` would have converted a write raku performs into a
throw.

The two are now computed independently in `vm_for_loop_body.rs`. The deep flag
belongs only to an *immutable QuantHash* source (`Set`/`Bag`/`Mix`), where
nothing reachable through the topic may be written; a provably-bare source is
shallowly readonly, because the topic has no container of its own even though
the item object it names may be perfectly mutable.

## `.first` was one missing call, not a missing mechanism

`.first`'s matcher already reaches `vm_call_on_value`, which consults
`CompiledCode::immutable_topic`, but the batched scan
(`try_first_match_batched`, the ~25x-cheaper setup-once path that actually
serves `.first({ ... })`) bound the topic with a direct `env` insert and never
called `set_loop_topic_readonly` the way the sibling grep loop does. Adding
`"first"` to `method_binds_immutable_topic` and the two-line mark to that scan —
under the same `ReadonlyFrameGuard` the grep loop opens, so the mark is
journalled rather than leaked — closes the row. `@a.first({ $_ = 5 })` is
unaffected: `@a` is not a provably-bare receiver, and that scan already binds
the element containers for it.

## A free second consumer: ADR-0045's rw bind

`ForLoopSpec::source_items_are_bare` also gates ADR-0045 slice 5 — an
`is rw` / `<->` parameter over a source that can only yield bare values has no
container to alias, and raku fails the *bind*, before the body runs. Widening
the oracle therefore closed that half too, with no separate change:
`for %h <-> $p`, `for %h -> $p is rw`, `for @a.List <-> $v`,
`for @a.pairs <-> $p`, `for @a.antipairs <-> $p`, `for %h.kv <-> $v`,
`for %h.List <-> $v` and `for (@a,) <-> $v` all now raise the same
`Parameter '$v' expects a writable container` rakudo raises, with the same
rendered value; `for $x, $y <-> $v { $v = 9 }` still binds and writes back.

The one residual approximation is *which* item is reported when a list literal
mixes shapes: `for $x, 2 <-> $v` names item 1 where rakudo names item 2, because
the verdict is one mark per loop rather than per item. Both die.

## Pinned

`t/immutable-topic-receiver-oracle.t`, 51 rows, green under `raku` as well as
mutsu — 18 of them controls that must NOT start throwing or start failing a
bind.

## Residue

`todo/deep/immutable-lvalues-that-mutsu-still-lets-you-assign-to.md` keeps the
rows a syntactic oracle cannot decide (a `@` variable `:=`-bound to a `List`, a
`$` variable holding a `Seq`), section B's surviving producer, and sections
C2/D/E/F. Two new rows were found while measuring: a `$^x` placeholder parameter
misses the readonly marking `-> $v` gets, and `%h.map({ .value = 9 })` throws
where `%h.pairs.map({ .value = 9 })` writes through.
