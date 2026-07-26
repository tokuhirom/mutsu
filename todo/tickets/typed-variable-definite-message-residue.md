# Three residual `:D`/`:U` message divergences on *variable* (not attribute) assignment

Found 2026-07-26 while fixing
`todo/tickets/attribute-default-typecheck-exception-type.md`. That ticket's
shared helpers (`got_type_name`, `value_short_repr`'s type-object arm,
`definite_type_check_assignment_error`) fixed the attribute paths and improved
the variable ones — `my Str:D $x = Int` now matches rakudo exactly — but three
cases on the *variable* declaration path are still off. They live in a different
dispatch (`src/vm/vm_var_assign_set_local.rs`) from the attribute check, which
is why they were not swept up.

## Repro

| declaration | raku | mutsu |
| --- | --- | --- |
| `my Int:D $x = Int` | `expected Int:D but got Int (Int) (perhaps Nil was assigned to a :D which had no default?)` | `expected Int but got Int (Int)` |
| `my Int:U $x = 5` | `expected Int:U but got Int (5)` | `expected Int but got Int (5)` |
| `my Str:D $x = Nil` | `expected Str:D but got Str (Str) (perhaps Nil was assigned to a :D which had no default?)` | `expected Str:D but got Nil (Nil)` |

Two distinct problems:

1. **The smiley is dropped from `expected` when the base type matches.**
   `my Str:D $x = Int` keeps `Str:D` (the value fails the *base* check, and that
   path reports the constraint verbatim), but `my Int:D $x = Int` and
   `my Int:U $x = 5` fail the *smiley* check and report only `Int`. So some
   definite/undefinite check is reporting a stripped constraint; find it and
   route it through `runtime::utils::definite_type_check_assignment_error`,
   which already appends rakudo's "perhaps Nil" hint under the right condition
   (`is_nominal_type_object_of`).

2. **`Nil` is not reported as the type object it resets to.** rakudo's
   `my Str:D $x = Nil` resets the container to `Str` and *then* fails the `:D`
   check, so `got` is `Str (Str)`. mutsu reports the raw `Nil`. The attribute
   side of exactly this was fixed by seeding the declared type object
   (`methods_object_default_ctor.rs`, the `Some(Expr::Literal(Nil))` arm); the
   variable side is `vm_var_assign_set_local.rs`'s
   `val.is_nil() && self.is_definite_constraint(&constraint)` branch (~line
   1146), which passes the `Nil` straight to the error builder.

## Pin to add

`t/attribute-default-typecheck.t` covers the attribute half. A variable-side pin
should assert all five shapes against raku: the three above plus the two that
already match (`my Str:D $x = Int`, `my Int $x = "s"`).

## Impact

Cosmetic — the exception type is already right (`X::TypeCheck::Assignment`) in
every case; only `.message` differs. No roast test asserts these strings today.
