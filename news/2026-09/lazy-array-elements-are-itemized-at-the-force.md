# A lazy source assigned to `@` now itemizes its elements

ADR-0040 makes a real `Array`/`Hash` itemize every element it stores, so an
element that holds a list reads back as `$(2, 3)`. Every eager construction
path did that; a lazy source assigned to an `@` variable did not:

```raku
my @a = lazy gather { take $_ for 1, (2, 3) };
say @a[1].raku;   # raku: $(2, 3)   mutsu: (2, 3)
```

The assignment stores the `LazyList` itself, so the store-side hooks see one
lazy value rather than a vector of elements — the elements first exist when
something forces the list. Found on 2026-09-02 by ADR-0064, which made
ADR-0040's itemization load-bearing for *reflection* (its slice/element
discriminator asks whether a value is a bare, non-itemized `List`) and so had
to carve out lazy-backed containers.

## The signal was already there

The ticket filed alongside ADR-0064 called this a representation question,
because the force site (`Interpreter::resolve_index_value`) is name-blind: a
`LazyList` is the reified form of BOTH a real `Array` assigned a lazy source
(elements ARE containers) and a bare lazy `Seq` (elements are the values), and
ADR-0040 slice 3 resolves that ambiguity from the variable's *sigil*.

It turned out not to need the sigil. `LazyList::array_context` already records
"bound/assigned into an `@` array slot" — it exists so `.gist`/`.WHAT` render
`[...]`/`Array` rather than `(...)`/`Seq` — and `force_lazy_list_vm` already
consults it for the sibling rule that an array-context element stores `Any`
rather than `Nil` (ADR-0049 slice 3). The itemization is the same rule from the
same flag, applied at the same place, so it went in right next to it:
`itemize_lazy_array_elements`, pre-scanned with
`Value::needs_element_itemization` so a flat sequence of scalars keeps the
vector it was handed with no rebuild.

Three readers were wrong and are now right — the subscript, a copy of the
forced array (`my @b = @a; @b[1]`), and a slice (`@a[0,1]`) — while a bare lazy
`Seq` keeps handing out bare elements (`my $s = lazy gather {...}; $s[1].raku`
is `(2, 3)`, and `.VAR` on it is identity). Iterating (`for @a -> $e`) was
already itemizing on its own path and is unchanged.

With the hole closed, ADR-0064's slice discriminator drops its carve-out, which
fixes the residual it was paying: `@a[*].VAR.^name` on a lazy-backed array is
`List` again, as raku says.

Pinned by `t/lazy-array-element-itemization.t`, whose 14 assertions pass under
real `raku` as well as under mutsu.
