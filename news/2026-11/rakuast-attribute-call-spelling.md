# RakuAST: `$.meth(args)` and `$.^meth` keep their spelling

A method call on `self` written with a `.`-twigil variable now views the way
rakudo's RakuAST frontend has it:

- `$.meth(1)`, `$.meth: 1`, `@.meth(1)` and `%.meth(1)` are a
  `Var::Attribute::Public` with `args`, deparsed as written.
- `$.^name` is `self.^name.item`.

Before, `$.meth(1)` viewed as the `self.meth(1)` it runs as, and `@.meth(1)` as
`@self.meth(1)`.

The parser marks the `self` of a `$.meth(...)` call, and the contextualized
call of `@.meth(...)` / `%.meth(...)`, with `Spelling::AttributeCall`. The
lowering maps a `Var::Attribute::Public` with arguments back to that call. The
calls run as before, `EVAL` of the view included. The view and lowering live in
`src/rakuast/attribute_call.rs`.
