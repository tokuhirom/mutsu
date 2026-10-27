# Dan: a user `[ ]` for `Whatever` on an instance, and `splice` on a punned role

Two fixes from `Dan`'s test suite (`t/02-dfr.t`, `t/04-upd.t`):

- A user `multi postcircumfix:<[ ]>(DataFrame:D $df, Whatever)` was skipped for an instance (the
  `Whatever` exclusion only meant to protect role-mixed Arrays), so `df[*]` fell through to the built-in
  and `df[*][1]` indexed a plain Array. Instances and punned role instances now reach the candidate, as
  they already did for `Int`.
- `Role.new.splice(...)` died with "Cannot resolve caller splice(...); Routine does not have any
  candidates" when the *role* (or a role it composes) declares `splice`: the `Any.splice` refusal only
  looked at classes. It now asks the composed roles too.

`t/03-dfo.t` still needs #15137 (`VarDeclaration::Anonymous` lowering).
