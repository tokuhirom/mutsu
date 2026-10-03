# RakuAST: named parameters, aliases and invocants cross the boundary

`.AST` refused most signature parameters beyond a plain positional or a bare
`:$x`. A survey of the `t/` files outside the round-trip ratchet found three
refusals behind the first failure of about 260 files:

- a typed, defaulted, `where`-constrained or required named parameter
  (`Int :$a = 3`, `Str:D :$c!`);
- a named alias (`:s(:$sort)`, `:foo($bar)`) or a named destructuring
  (`:$p ($q, $r)`);
- an invocant, written (`$self:`) or synthesized (`Foo:D:`). The parser marks
  it with an `invocant` trait, so the converter reported it as a custom trait.

Measured on rakudo 2026.09, a named parameter is one `RakuAST::Parameter`
whose `names` list holds every name it binds under, innermost first:
`:a(:b(:$c))` is `names => ("c", "b", "a")` with the target `$c`, and
`:x(:y($z))` is `names => ("y", "x")` with the target `$z`. Its type, default,
`where` clause and `!`/`?` marker are that node's own fields. The parser keeps
an alias as a chain of `ParamDef`s instead. `rakuast::named_param` flattens the
chain on the way out, and on the way in it rebuilds the chain with the
markers on the outermost level, where the binder checks them. An invocant
renders as `invocant => True` after the type and type captures. A synthesized
invocant has no target.

Three smaller gaps closed with it:

- Lowering accepted only `Type::Simple`. `Type::Definedness`, `Type::Coercion`
  and `Type::Parameterized` now lower back to the parser's type spelling
  (`Str:D`, `Int()`, `Hash[Str, Int]`) at every type site
  (`rakuast::type_lower`).
- An untyped `@`, `%` or `&` parameter no longer carries the implicit
  `Type::Setting(Any)`. Rakudo gives it only to `$` and sigilless parameters.
- A `::T` capture declares `T` as a type name, so the body's bareword `T`
  renders as `Type::Simple` instead of refusing.

The round-trip ratchet grows from 2502 to 2713 of 6066 `t/` files. Pinned by
`t/rakuast/rakuast-named-parameters.t` and
`t/rakuast/rakuast-invocant-parameters.t`. Both pass under mutsu and raku.
