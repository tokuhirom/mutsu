# The write-through calls have a typed reader

`lvalue_call::classify(callee, args)` reads a write-through call (`$o.attr = v`, `f(...) = v`,
`$code(...) = v`, `f(...)++`) as an `LvalueCall` whose arguments have names (`invocant`, `method`,
`method_args`, `value`, `routine`, `callable`), and the compiler, the TRIR front end and the RakuAST
writeback no longer index `args[0]`, `args[2]` or `args[3]` themselves (ADR-17198 step 2b). The
argument layout is now written once in `src/lvalue_call.rs`, beside the constructors.
