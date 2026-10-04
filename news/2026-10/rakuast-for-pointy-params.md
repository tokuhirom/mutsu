# RakuAST: every `for` pointy-block parameter survives EVAL

After the method-literal slice, 47 `t/` files that `.AST` accepted were
refused by EVAL at a `PointyBlock`. Each was a `for` loop with several
parameters, such as `for %h.kv -> $k, $v { … }`. The lowering took only a
single parameter, and only its name.

Taking only the name was also a wrong answer. The parser keeps a single loop
variable's full `ParamDef` (type, traits, sigil) in `Stmt::For.param_def`.
The lowering left that field empty, so `for 1, "a" -> Int $a { … }`
type-checked the `"a"` when parsed directly and accepted it after the round
trip.

The lowering now reads the pointy block's signature through the same
parameter lowering every routine uses. It puts the result where the parser
does: one parameter in `param` / `param_def`, several in `params` /
`params_def`. Typed, `is copy`, `is rw` and `@`-sigilled loop variables bind
as they did before the round trip.
