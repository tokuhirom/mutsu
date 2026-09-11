# Shared role proto declarations are not re-registered through a diamond

When a class or grammar composed two roles that both inherited the same role,
mutsu ran the shared ancestor's deferred body once per composition path. A
`proto rule`, `proto token`, or `proto regex` in that body was therefore
mistaken for a second declaration and raised `X::Redeclaration`.

Ancestor role bodies now use the same `(composing type, role)` composition memo
as direct role bodies. This keeps candidates contributed by the ancestor and
child roles available while preserving genuine redeclaration errors within one
body. The regression coverage is in
`t/oo/role/role-proto-diamond.t` and
`t/oo/role/role-proto-diamond-class.t`.

Fixes [#7901](https://github.com/tokuhirom/mutsu/issues/7901).
