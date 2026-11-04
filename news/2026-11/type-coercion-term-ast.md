# `Int()` / `Int(Str)` are `Type::Coercion` in term position

`Int()`, `Int(Str)` and `K()` for a declared class are a `RakuAST::Type::Coercion` in rakudo's
tree (`base-type`, plus `constraint` when written). mutsu rendered them as an
`ApplyPostfix(Type::Simple, Call::Term)` and could not lower a hand-built `Type::Coercion` in
term position at all (`EVAL does not yet support lowering RakuAST::Type::Coercion`). Both
directions now agree with rakudo; a call whose argument is a value (`Int("5")`) stays a call. The
`foo()` of a type shadowed by a same-named sub (the parser's `foo(Any)` package literal) is the same
node and lowers back to the literal, which also lets `t/modules/module-type-wins-over-same-named-sub.t`
pass as `EVAL($ast)` (added to the ratchet). Found by the #16356 triage.
