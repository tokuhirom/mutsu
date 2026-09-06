# An `@`-sigil attribute's value is not list-assigned: `has @.w = 1..3` keeps the Range as one element

Found 2026-09-06 while exploring the neighbourhood of the attribute-default
trailing-comma fix (`news/2026-09/one-parameter-pointy-block-loses-its-sigil.md`).
It is a separate bug: no comma is involved and it reproduces on the simplest
possible declaration.

Raku assigns to an `@`-sigil attribute exactly the way `my @a = …` assigns:
list assignment. Anything `Positional`/iterable flattens into the container, a
`Hash` flattens to its pairs, and a plain scalar becomes a one-element array.
mutsu does none of that consistently:

```raku
class W { has @.w = 1..3 }       ; say W.new.w.raku   # raku: [1, 2, 3]   mutsu: [1..3,]
class Z { has @.z = (1,2,3).Seq }; say Z.new.z.raku   # raku: [1, 2, 3]   mutsu: [(1, 2, 3).Seq,]
class Y { has @.y = (1,2,3).List}; say Y.new.y.raku   # raku: [1, 2, 3]   mutsu: [(1, 2, 3),]
class I { has @.i = %(x=>1) }    ; say I.new.i.raku   # raku: [:x(1)]     mutsu: [{:x(1)},]

class F { has @.a }
say F.new(a => 5).a.raku;      # raku: [5]     mutsu: 5
say F.new(a => Any).a.raku;    # raku: [Any]   mutsu: Any
say F.new(a => (1..3)).a.raku; # raku: [1, 2, 3]  mutsu: [1, 2, 3]   (this one is right)
```

Two distinct halves, and they pull in opposite directions:

1. **The default path over-wraps.** `src/parser/stmt/decl/has_decl.rs` ends the
   `has` parse with a blanket `sigil == b'@'` rewrite that wraps any default
   expression that is not already array-*shaped* into `Expr::ArrayLiteral(vec![
   other])` (the `Expr::ArrayLiteral(_) | BracketArray | ArrayVar | Var | Index`
   match, currently around line 942). At parse time it cannot know whether the
   expression will produce a list, so a `Range`, `Seq`, `List`, `Hash` or a
   list-returning call becomes a single element of a one-element array. The
   runtime coercion that should have handled this,
   `Interpreter::coerce_attr_value_by_sigil` (`src/runtime/methods_signature.rs`),
   already has correct `Range`/`RangeExcl`/`Seq`/`Array` arms — they just never
   see the real value, only the `ArrayLiteral` wrapper the parser built.

2. **The supplied-value path under-wraps.** The same
   `coerce_attr_value_by_sigil` falls through with `_ => val.clone()` for a
   plain scalar or a type object, so `F.new(a => 5)` stores the bare `Int` in an
   `@`-sigil attribute instead of `[5]`.

The obvious fix is therefore one change on both sides: drop the parse-time wrap
and let the sigil coercion do the whole job, adding the missing
scalar/type-object → one-element-array arm (and a `Hash` → pairs arm) to it.
That is why this is a ticket and not a one-liner: `coerce_attr_value_by_sigil`
is on four construction paths (`methods_object_dispatch_new.rs`,
`methods_object_default_ctor.rs`, `methods_dispatch_new.rs`,
`attr_build_defaults.rs`), it is shared with `bless`/`BUILD`-supplied values,
and it deliberately passes a *deferred* `Seq` through unmaterialized (see the
long comment on its `Seq` arm), which a blanket `_ =>` wrap would break. The
shaped-array branch of the parse-time wrap
(`shaped_array_new_with_data_expr`) must also keep working.

A visible knock-on: `has @.j = 1, 2 ... 6` is `[1, 2, 3, 4, 5, 6]` in raku but
mutsu stores the merged sequence as one unflattened element. The comma-splitting
half of that was fixed on 2026-09-06 (the attribute initializer now runs the
shared `normalize_comma_list_items`); only the flattening described here is
left.

Minimal repro: any line in the block above. A pin belongs next to
`t/attr-default-trailing-comma.t`.
