# `is rw` (or bare `%!attr`) accessor: whole-hash assignment merges instead of replacing

Found while pinning the "bare `@!attr`/`%!attr` method allows indexed
assignment without `is rw`" fix (DBIish's `column-types` battery-bundling
work) — a **separate, pre-existing** bug, reproducible on plain `main` with an
explicit `is rw` accessor (nothing to do with that fix):

```raku
class HashAttr {
    has %!info = (a => 1, b => 2);
    method info is rw { %!info }
}
my $h = HashAttr.new;
$h.info<a> = 99;
$h.info = { z => 42 };
say $h.info;
# raku:  {z => 42}
# mutsu: {a => 99, b => 2, z => 42}
```

A genuine whole-value assignment (`$obj.info = {...}`) should **replace** the
attribute, not merge the new hash into the old one. The Array equivalent
(`method items is rw { @!items }`, `$obj.items = <p q r>`) correctly replaces —
this is Hash-specific.

## Where

`src/runtime/methods_mut.rs::normalize_rw_accessor_assignment` — the
`Some(ValueView::Hash(existing_hash))` arm always calls
`Self::normalize_hash_like_assignment(existing_hash.map.clone(), value)`, which
merges the existing map with the incoming value's map. That merge is the right
behavior for an *indexed* round-trip (`$o.h<k> = v` reads the current whole
hash, changes one key, writes the whole thing back — the "unrelated keys must
survive" case), but wrong for a genuine whole-value replacement, which should
just install `value` verbatim (after `.descalarize()`).

The caller (`assign_method_lvalue_with_values` in
`methods_mut_method_lvalue.rs`) currently cannot tell these two shapes apart
from this function's inputs alone — both arrive as `method_args.is_empty()`
with some `value`. Distinguishing them needs either a flag threaded down from
the two call sites (indexed-assignment desugaring in
`builtins_multidim_assign.rs` vs. a direct `$obj.method = value` compile), or
recognizing that an indexed round-trip always re-reads the *whole* container
first specifically to preserve it, so passing that intent explicitly is
probably simplest.

## Why not fixed here

Orthogonal to the fix this ticket was found alongside (indexed assignment into
a bare-body accessor), and touches the general `is rw` accessor-assignment
path other roast tests already rely on — needs its own careful before/after
check against `t/`, not a drive-by change.
