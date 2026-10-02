# Unbounded ranges are lazy through one mechanism, for every element type

Each operation on an upward-unbounded Range used to decide laziness for
itself, and most decided it with an "is the start an `Int`?" check. `1..*`
was lazy almost everywhere. `1.5..*`, `1e0..Inf` and `"a"..*` reified a
capped prefix: 100,000 elements for array contexts, 1,000,000 for
`value_to_list`. That made `.head`, `[^n]`, `.first`, `.list`, `.Seq`,
`for`, `my @a = ...`, scans, zips and the `.map`/`.grep` pipes over a Str
range take 0.25–2 s. Past the cap the answers were wrong:
`(1..*).first(* > 1_000_005)` was `Nil`, `for ^Inf` stopped at 1,000,000,
`(1.5..*)[1_000_001]` was `Nil`, `(1.5..*).AT-POS(2)` was `Nil`, and
`[\+] 1.5..*` started `1 3 6`.

`runtime::unbounded_range` is now the single definition:

- `first` gives the effective first element of any unbounded range.
- `nth` gives random access `first + i` for a numeric start, in the start's
  own type.
- `lazy_list` turns the range into a reify-on-demand LazyList whose new
  `SequenceSpec::Succ` steps by the shared `.succ` primitive.
- `Steps` walks the range in O(1) memory.
- `pipe_source` decides what a lazy pipe pulls from.

The integer-only special cases now route through it:
`infinite_int_range_to_lazy_array`, `.List`'s `infinite_arithmetic_list`, the
per-type GenericRange arms of `pull_source_element`, the `.head`, `.first`,
subscript and `for` paths, `coerce_to_array`, `range_at_pos` and the scan
source. Every `end.to_f64()`-based "is it infinite" predicate now defers to
`is_infinite_range`; those predicates read the `*` end of `"a"..*` as `0`.

Two problems that already existed turned up along the way. A lazy `for` over
a sequence spec forced one element per iteration and copied the whole prefix
each time, which made it quadratic. It now forces in doubling batches. And
`@lazy[2..*]` on an infinite list tried to force an endless prefix (which
hung `my ($a, *@r) := (1, 2 ... *)`); it is now a lazy `.skip`.

The related findings left open are #10780 (quadratic `for` over a `.map`
pipe or gather), #10781 (`[*-1]` on a lazy list) and #10782 (`.iterator`
on a lazy source).
