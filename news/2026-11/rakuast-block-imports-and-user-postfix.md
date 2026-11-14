# RakuAST: a pushed scope sees its `use` imports; user postfix operators stay `ApplyPostfix`

Two gaps stopped Red's optional RakuAST compiler (`red-compile`, run as a
`CHECK` phaser over `$*CU`) before any of its tests ran.

A resolver that pushed a block's scope (`$resolver.push-scope`) answered
`resolve-lexical` only with what the block declares itself, so a routine a
`use` inside the block imported was not found. Red asks
`resolve-lexical('&__RED_OPERATOR_LOADED__')` to decide whether a compiled
`.map` may use its operators, and t/092 failed its "Compilation preserves
imported operators" check. The parser now keeps the routine names each `use`
imported on the statement (`Stmt::Use::imports`), the RakuAST `Statement::Use`
carries them in a hidden field, and a pushed scope declares them as
`Declaration::Import`, the way rakudo's pad does.

A user postfix operator applied as an operator (`5!`, or `$x++` with a user
`multi postfix:<++>` in scope, as Red exports one) was viewed as a call,
`Call::Name(postfix:<!>)`, where rakudo keeps the operator application
`ApplyPostfix(Postfix(operator => "!"))`. Red then reported `{ .number++ }` as
an unsupported function call, and t/091's CHECK died. The parser now spells the
operator form, the view keeps the application, and lowering calls the user
routine again. A hand-built `Postfix` node of an operator other than `++` or
`--` now lowers to its `postfix:<op>` routine; it used to be read as the
same-spelled prefix and evaluate to `Nil`.
