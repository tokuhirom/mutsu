# `$_.VAR.^name` reports the item's own type for a topic aliased to a literal

```
$ raku  -e 'for 1,2 { say $_.VAR.^name; last }; my %h = a=>1; for %h.keys { say $_.VAR.^name; last }'
Int
Str
$ mutsu -e '…'   # was: Scalar / Scalar
Int
Str
```

The ticket's `given 5` half had already been fixed by the time this was picked
up (`given 5 { $_.VAR.^name }` reported `Int` on `main`). What remained was the
`for` half, and it had two independent causes.

## 1. The topic of a literal-list `for` was never marked immutable

`.VAR` keys off the readonly *kind*: a name recorded as
`ReadonlyKind::Immutable`/`ImmutableValue` has no container, so `.VAR` returns
the value itself. The marking was there for `given 5` and for `for 1..2` (the
int-range loop marks unconditionally) but not for the eager list loop: its
`topic_readonly` in `src/vm/vm_for_loop_body.rs` was true only for an
*immutable QuantHash* source (`Mix`/`Set`/`Bag`), which requires a named
container binding.

Raku's actual rule is per item — the topic is writable exactly when the item is
a container:

| source | `$_.VAR.^name` (raku) |
| --- | --- |
| `for @a` | `Scalar` |
| `for @a.values` | `Scalar` |
| `for $a, $b` | `Scalar` |
| `for @a[0..1]`, `for @a.map({…})` | `Scalar` |
| `for 1, 2`, `for (1,2)`, `for <a b>` | `Int` / `Str` |
| `for %h.keys` | `Str` |
| `for %h` | `Pair` |

mutsu cannot decide that at runtime, because real `Array`/`Hash` elements are
stored **bare** (`todo/deep/element-itemization-lost-in-scalar-binding.md`,
ADR-0040) — a `!item.is_container_ref()` test would mark the slice and `.map`
rows read-only too, inventing throws `raku` does not have. So the flag is a
*provable* compile-time property instead: `ForLoopSpec::source_items_are_bare`,
set by `Compiler::for_iterable_yields_bare_items` for an `ArrayLiteral` whose
every element is an `Expr::Literal` (`for 1, 2` / `for (1,2)` / `for <a b>`) and
for `.keys` on an `@`/`%` variable. A mixed list (`for 1, $a`) stays writable —
lax, but never wrong. `for %h` (yielding immutable `Pair`s) and the
`map`/`grep`/block-argument topics still diverge and are recorded in
`todo/tickets/immutable-lvalues-that-mutsu-still-lets-you-assign-to.md`.

This also makes `for 1,2 { $_ = 5 }`, `for (1,2) { … }`, `for <a b> { … }` and
`for %h.keys { … }` throw `X::AdHoc` "Cannot assign to an immutable value", as
`raku` does — four rows of that survey ticket.

## 2. `.VAR`'s per-name meta cache answered for the wrong binding

Even with the marking in place, a `.VAR` on a bare topic could still come back
`Scalar`: `call_method_mut_with_values`'s `VAR` arm probed the
`__mutsu_var_meta::<name>` cache *before* the readonly-kind gate, and the cache
is keyed by name alone. Every topic shares the key `_`, so
`for @a { .VAR }` built a `Scalar` meta object that `for 1,2 { .VAR }` then
returned verbatim — the divergence was invisible in a one-liner and only showed
up in a file that probed several loops. The readonly-kind gate now runs first:
"this name has no container right now" is a live property of the current
binding, not something a per-name cache may answer.

Pinned by `t/itemization-and-readonly.t` (including the cache-ordering case),
which passes under real `raku`.
