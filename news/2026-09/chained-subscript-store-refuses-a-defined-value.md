# A chained subscript store no longer autovivifies over a defined value

Autovivification applies to an *undefined* slot. Rakudo refuses a chained
subscript store that would have to descend through a slot already holding a
defined value with no writable container behind it:

```
my @a = 1,2,3;   @a[1][0] = 9     X::Assignment::RO  "Cannot modify an immutable Int (2)"
my @a = 1,2,3;   @a[1]<k> = 9     X::AdHoc           "Type Int does not support associative indexing."
my @a = (1,2),3; @a[0][0] = 9     X::Assignment::RO  "Cannot modify an immutable List ((1 2))"
my %h = a => 1;  %h<a>[0] = 9     X::Assignment::RO  "Cannot modify an immutable Int (1)"
```

mutsu did none of that. Every vivify decision below the root asked only
"is this an `Array`, a `Hash` or a `ContainerRef`?", and answered "vivify me"
for everything else — so a defined `Int` in the slot was silently replaced by a
fresh container (`@a` became `[1 [9] 3]`), an immutable `List` was written
through in place, and a *hash* root lost the write altogether, because
`assign_into_nested_container` no-ops on a non-container target and nothing
noticed.

This was section C2 of the immutable-lvalue survey
([#7556](https://github.com/tokuhirom/mutsu/issues/7556)), and the survey had
already identified the shape of the fix: the ROOT probe in
`exec_index_assign_expr_nested_op` draws exactly this line correctly —
`root_needs_viv` deliberately excludes a defined value, with a comment saying
raku dies there and that papering over it would clobber the value. The same
distinction was simply missing one level down.

`Interpreter::subscript_descent_refusal` is that distinction, factored once and
consulted at all three vivify sites: the two-level chained store through an
array root, the same store through a hash root, and the intermediate step of
the 3+ level walk. It classifies the slot rather than the syntax — a mutable
`Array`/`Hash`, a `:=`-bound cell, a `Proxy` and an object that owns its own
element storage (a `Buf`, a class with its own `AT-POS`) are all legitimate
descent targets and are left exactly as they were; `Nil`, a type object and an
absent slot are the only shapes that genuinely autovivify; everything else is
refused. The refusal carries rakudo's own two classes — `X::Assignment::RO` for
a positional outer subscript, `X::AdHoc` with the `Any.AT-KEY` wording for an
associative one — and renders the offending value with `gist_value`, which is
what makes a `List` read `(1 2)` and a `Set` read `Set(1 2)` instead of their
space-joined string coercion. Every message in the new test is byte-for-byte
what `raku` prints.

One shape is deliberately left alone with a `TODO`: a reifiable sequence (`Seq`,
`LazyList`, `Slip`) in the slot. Rakudo reifies it and then refuses the
*element* ("Cannot modify an immutable Int (3)"), which a predicate over the
slot's current value cannot do.

Pinned by `t/chained-subscript-store-refuses-defined-value.t`, whose 26
assertions pass unchanged under `raku` as well as under mutsu — the refusals and
the autovivifications that must keep working (`@a[1][0]` on an empty array, a
type-object slot, a `Nil` slot, a mutable `Array` element, a `:=`-bound element
cell, and the deep walk through an empty `Array`).
