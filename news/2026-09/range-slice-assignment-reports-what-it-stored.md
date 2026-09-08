# A Range slice assignment reports the list it stored

`(@d[0..0] = 5)` answered the bare `5` where rakudo answers `(5,)`:

```raku
my @d; say (@d[0..0] = 5).raku;    # raku: (5,)   mutsu (before): 5
my @d; say (@d[0..^1] = 5).raku;   # raku: (5,)   mutsu (before): 5
```

This is the sibling of the `@d[0,]` comma spelling fixed in #7589, with a
different root cause — which is why it was left out of that PR rather than
widening it.

## Root cause

A Range subscript on a plain (non-native, non-hash) positional array was **not**
expanded into an index list: the `expand_range` gate in
`src/vm/vm_var_assign_index_named.rs` was limited to hash variables and native
typed arrays. So the Range never reached the `ValueView::Array(keys, kind)`
slice arm that builds `assigned_values`, and fell through to the shared tail,
where `idx_is_single_element` is false for a Range and the fallback simply
returns the raw RHS.

That is why the multi-element case *looked* right: `@d[0..1] = 5, 6` returned the
RHS list `(5, 6)`, which happens to equal the stored list. With one slot the raw
RHS is the bare `5` and the accident stops working — and a short RHS exposed the
same gap the other way, `@d[0..2] = 5,` answering `(5,)` instead of rakudo's
padded `(5, Any, Any)`.

## The fix

The second of the two shapes the ticket sketched, the one that removes a
duplicate rule rather than adding one: `expand_range` now also covers a plain
positional array, so a finite Range subscript becomes the index list the
existing slice arm already handles correctly — for the storage *and* for the
value it reports.

It is guarded on a finite end (`b != i64::MAX && b >= 0`), the same precedent the
Buf slice arm nearby uses, so an infinite or `Whatever`-ended Range (`@a[0..*]`,
`@a[^Inf]`) is still never enumerated.

A bonus from routing through the one arm: the short-RHS *storage* padding lines
up with rakudo too. `my @d; @d[0..2] = 5,` used to leave `[5, Nil, Nil]` and now
leaves `[5, Any, Any]`.

## Pins

`t/range-slice-assign-rvalue.t` covers the three one-element Range spellings
(`0..0`, `0..^1`, `^1`), the padded short RHS, the multi-element and
single-index controls, the `@d[0,]` comma spelling from #7589, and three
storage assertions. All ten were checked against rakudo.

`t/range-slice-assignment.t`, `t/slice-assign-generic-range.t`,
`t/hash-range-slice-assign.t`, `t/slice-assign-pads-short-rhs.t`,
`t/slice-assign-one-element-rvalue.t` and `roast/S09-subscript/slice.t` pass
unchanged.

## Left open

A `Whatever`-ended Range slice assignment (`@d[1..*] = 9`) grows the array to
100002 elements instead of clipping to its current length. That reproduces
identically with and without this change — verified by building both ways — so
it is pre-existing and filed separately as
[#7674](https://github.com/tokuhirom/mutsu/issues/7674) rather than widening
this fix.

Closes [#7651](https://github.com/tokuhirom/mutsu/issues/7651).
