# `for @a[$subscript]` decides slice-vs-element from the subscript's runtime value

`@arr[0..2]` correctly slipped its elements one-per-iteration into a `for` loop, but the
same Range (or an Array of indices) held in a variable did not:

```raku
my @numbers = <4 8 15 16 23 42>;
.say for @numbers[0..2];    # 4 / 8 / 15   -- was already right
my $range := 0..2;
.say for @numbers[$range];  # printed "(4 8 15)" as ONE item
my @range = 0..2;
.say for @numbers[@range];  # printed "(4 8 15)" as ONE item
```

## Root cause

The subscript expression itself was fine — `@numbers[$range].elems` was already `3`, and
`my @copy = @numbers[$range]` already copied three elements. The divergence lived entirely
in the compiler's `for`-loop rewrite.

`Compiler::desugar_for_scalar_element_source` (`src/compiler/stmt.rs`) rewrites a
`for @a[i] { ... }` whose source is a var-rooted container *element* into

```raku
my $tmp = @a[i];   for $tmp { ... };   @a[i] = $tmp;
```

so that the loop body's `$_` rw-aliases that element (`for @a[i] { .=Int }` mutates
`@a[i]`), reusing the existing scalar-source writeback instead of new VM machinery. To
avoid collapsing a real slice into one topic, it bailed out for slice subscripts — but the
bail-out predicate, `for_index_is_slice`, was purely *syntactic*: a literal `Range`, a
comma list, or a bare `*`. A subscript that merely *held* a Range or a list at runtime
(`$range`, `@range`, a function call, `%h{@keys}`) fell through to the scalar rewrite, so
`$tmp` became one item and the loop ran once. The unconditional writeback made it worse:
`for @c[$r] { }` over an out-of-range bound Range padded the array with `Any`s, and
`for @b[$r] { $_ = $_ * 10 }` splattered the last topic across the selected range
(`[20, Nil, 3, 4]`).

Whether a subscript selects one element or a slice is a property of its runtime value —
Rakudo dispatches its `postcircumfix:<[ ]>` candidates on `Iterable` — not of the syntax
that produced it.

## Fix

`desugar_for_scalar_element_source` now makes that decision at runtime. A *literal*
subscript (`@a[2]`, `%h<k>`) is statically known to be a single element and keeps the cheap
guard-free rewrite; every other shape is compiled as

```raku
my $idx   = <INDEX>;            # evaluated exactly once
my $slice = $idx ~~ Iterable;   # Rakudo's own slice/element rule
my $tmp   = $slice ?? @a[$idx].Slip !! @a[$idx];
for $tmp { ... };               # a Slip flattens when iterated, a plain value does not
unless $slice { @a[$idx] = $tmp }
```

The `Slip` is what carries the runtime decision into the loop: a scalar holding a `Slip`
flattens when iterated while any other value (including an `Array` element that happens to
be a list) stays a single topic, so no new opcode was needed. The slice branch skips the
aliasing writeback, which also stops the array padding and the topic splatter described
above. The syntactic `for_index_is_slice` check survives only as a fast path that keeps
literal slices on the untouched plain compilation.

Supporting fix: `Value::does_role_hierarchy` (`src/value/types_isa.rs`) claimed a `Range`
does not do `Iterable`, so `(0..2) ~~ Iterable` was `False` (Raku: `True`) — even though the
type-name table in `signature.rs` already listed `Range` under `Iterable`. `Range`
(all four exclusivity variants plus `GenericRange`), `Slip`, `HyperSeq`/`RaceSeq` and the
`Array`/`List`/`Range`/`Seq`/`Slip`/`Hash`/`Map` type objects were added. `Set`/`Bag`/`Mix`
were deliberately left out: they do `Associative`, and `(1,2).Set ~~ Iterable` is `False` in
Raku too.

## Test

`t/array-slice-variable-subscript.t` (29 assertions, passes under both `raku` and `mutsu`):
literal / bound-Range / Array / list-literal / bound-list subscripts, iteration counts and
`.elems` for each, a single `Int` variable subscript and an element that is itself an Array
(neither may flatten), Hash slices by an Array of keys versus a single key in a variable,
rw aliasing of a single element by both a variable and a literal subscript, and a read-only
slice loop that must leave its array untouched.

## Left open

`my $assigned = 1..3; @n[$assigned]` should be a *single* index (Raku itemizes a scalar
assignment, and an itemized Range numifies to its element count), but mutsu keeps the bare
`Range` for a named scalar and slices. The anonymous `my $ = 1..3` form already works.
Recorded as `todo/tickets/range-assigned-to-named-scalar-not-itemized-as-subscript.md`.

Rw aliasing through a *slice* (`for @a[0..1] { $_ = $_ * 10 }` mutates `@a` in Raku) is
still unimplemented in mutsu; it was already absent for literal slices and this change
keeps the two paths consistent rather than adding a half-aliasing slice case.
