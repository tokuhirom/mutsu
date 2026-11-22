# `temp` and `let` over a non-container throw `X::Localizer::NoContainer`

`temp &f.wrap({ ... })`, `temp 5` and `let &f` were not read as the `temp` /
`let` prefix. The statement parser handled only a variable, an element or a
method lvalue, and anything else fell through to a call to a sub named `temp`
("Unknown function: temp"), or to a parse error for `let`.

Now the prefix binds to its operand as tightly as a symbolic unary operator,
as in Rakudo. The operand is evaluated first (so the wrap in
`roast/S06-advanced/wrap.t` still happens), then `X::Localizer::NoContainer`
is thrown. `X::Localizer::NoContainer` now has Rakudo's message, "Can only use
'temp' on a container" (or 'let'). The `.AST` of such a statement is an
`ApplyPrefix` over the operand, as in Rakudo, and `EVAL` of that tree throws
the same way (#18623).

A call to an `is rw` routine (`temp f() = 5`) still lands on this path. In
Rakudo it yields a container. The TODO on `Target::NoContainer` records this.
