# RakuAST: attribute traits and defaults cross the boundary

`has $.x is rw` was refused by `.AST` as an "attribute with traits" -- one of
the most frequent refusals under `MUTSU_RAKUAST=1` -- and an attribute default
(`has $.x = 5`) rendered but could not be lowered back, so every class with a
defaulted attribute failed to round-trip.

Measured on rakudo 2026.09, a written attribute trait is a
`Trait::Is(name => Name.from-identifier("rw"))` in the declaration's `traits`
list, and an `= EXPR` default adds an implicit `Trait::WillBuild` after it.
The default is also the `initializer`. The gist leaves the implicit trait out,
and only `.traits` answers it. mutsu printed it in the gist, so
`t/rakuast/rakuast-attribute-default.t` had stopped passing under raku; the
test now checks `.traits`, and both run it the same.

The new `rakuast::attribute` renders and lowers `is rw`, `is readonly` and
`is required`, and an `Initializer::Assign` lowers back to the attribute's
default. `is readonly` was previously dropped from the rendered node. The
parser keeps these traits as flags, not in source order, so an attribute with
more than one of them stays refused. So do `is default(…)`, whose value the
parser also copies into the initializer, and `is built`, whose argument it
does not keep.

The round-trip ratchet grows from 1156 to 1194 of 5764 `t/` files. Pinned by
`t/rakuast/rakuast-attribute-traits.t`, which passes under both
mutsu and raku.
