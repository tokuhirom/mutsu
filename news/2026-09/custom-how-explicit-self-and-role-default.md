# Custom HOW methods keep explicit `$self` separate from their invocant

`Type.^method` now dispatches through the type's HOW and passes the type object
as the method's first explicit argument. This matters to Red's declaration-time
custom HOW methods, which use an ordinary `Mu:U $self` alongside their implicit
HOW invocant.

Method binding keeps that explicit `$self` in its own lexical slot, including
the general binder used by multi methods. Role attribute defaults evaluated
during composition also see the value being composed rather than the enclosing
HOW method's invocant.

Pinned by
`t/oo/role/custom-how-explicit-self-and-role-default.t`, and advances the Red
runtime frontier tracked by [#7988](https://github.com/tokuhirom/mutsu/issues/7988).
