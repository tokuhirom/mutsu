# A native-int attribute store through its accessor now wraps on overflow

`has uint8 $.v is rw` declares an 8-bit slot, but assigning through the
accessor never narrowed what it stored:

```raku
class Uns { has uint8 $.v is rw }
my $u = Uns.new; $u.v = 260; say $u.v;   # mutsu: 260, raku: 4
```

The same value stored into a same-width lexical (`my uint8 $x = 260`) wrapped
correctly, and so did a `has int8 $!v` private attribute's `++`/`--` tail once
[#8985](https://github.com/tokuhirom/mutsu/issues/8985) was fixed. Only the
accessor route was wrong — and it was wrong for every spelling of it: from
inside a method (`method set { $.v = 260 }`), from outside (`$obj.v = 260`),
for a compound assignment (`$.v += 10`), and for a hand-written `is rw` method
exposing the attribute (`method v is rw { $!v }`).

## Root cause

One missing step, in two store paths that each reach the attribute a different
way.

`$obj.v = 260`, `$.v += 10` and `method v is rw { $!v }` all funnel through
`assign_method_lvalue_with_values`
(`src/runtime/methods_mut_method_lvalue.rs`), whose single pre-store chokepoint
is `check_attr_store_type` (`src/runtime/methods_mut_rw_attr.rs`). A plain
`$.v = 260` *inside a method* does not: it compiles to the by-name
`AssignExpr` opcode and lands in `exec_assign_expr_op_inner`
(`src/vm/vm_misc_assign.rs`), which already grew an attribute-constraint
fallback of its own for exactly this shape.

Both of them only *type-checked* the incoming value, and `type_matches_value`
accepts any `Int` for a native-int constraint — `260` is a perfectly good
`Int`, so it passed for `uint8` and reached the attribute untouched. Nothing
else on either path would have narrowed it.

The ordinary local-slot store does not have this gap: `exec_set_local_op_inner`
(`src/vm/vm_var_assign_set_local.rs`) runs `wrap_native_int_by_constraint`
right after its own type check, for exactly the same reason. That is also why
the private spelling `$!v = 260` was already correct — a `$!` attribute local
is a compile-time-baked slot, so it goes through that store and never reaches
either of the two paths above. The only thing separating the right answer from
the wrong one was which opcode the source text happened to compile to.

## Fix

Both paths now take the local-slot store's last step.

`check_attr_store_type` returns the value as the attribute will actually hold
it, applying `wrap_native_int_by_constraint` once the type check passes. It is
the same routine and the same position in the sequence as the local-slot
store's, so the two spellings of one store agree by construction rather than by
duplicated logic, and both callers of the chokepoint (the generated accessor
and the `is rw` method that exposes the same attribute) pick it up together.

`exec_assign_expr_op_inner` applies the same call after its own type check and
coercion. That also closes the gap for an ordinary typed lexical reached by
name rather than by slot, which had been narrowing-free for the same reason.

Because it is the shared native-store narrowing step and not an int-only patch,
`num32`'s store-time truncation to single precision comes along with it:
`$obj.f = 1.1e0` on a `has num32 $.f is rw` now stores `1.100000023841858e0`,
matching rakudo, where it previously kept the full double.

`wrap_native_int_by_constraint` was `pub(super)` within `src/vm/`; it is
`pub(crate)` now so the runtime's attribute path can call the one
implementation instead of growing a second. There are three callers of it and
one narrowing rule.

Pinned by `t/nativecall/native-int-attr-accessor-assign-wraps.t`, whose twelve
assertions all pass unchanged under rakudo: the four accessor spellings, signed
and unsigned narrow widths, a negative value into `uint8` and into full-width
`uint`, the `num32` truncation, and three negative controls (an in-range store,
an untyped attribute, and a boxed `Int` attribute must not wrap).
