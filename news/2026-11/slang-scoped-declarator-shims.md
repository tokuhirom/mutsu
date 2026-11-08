# A slang declarator can open a method scope (Qwiratry `aggsub`)

The slang host now serves the core rules a declarator production like
Qwiratry's `aggsub` calls: `<.enter-block-scope('Method')>` /
`<.leave-block-scope>` (`$*BLOCK` is the `RakuAST::Method` it builds),
`<.identifier>`, `<typename>`, `<colonpair>`, `<blockoid>` (the core block's
`Blockoid`), `.Nodify('ParseTime')`, and the `$*R` / `$*CU` the production
reads. `RakuAST::VarDeclaration::Implicit::Self.new`, `Routine.replace-signature`
and the no-op `to-parse-time` / `to-begin-time` exist. A statement-prefix
production's expression is listed as a `Statement::Expression` in the body it
sits in, so `$<body>.ast.DEPARSE` prints valid code, and `DEPARSE` works inside
the slang host's interpreter.

Three general fixes came out of it, each checked against rakudo: a role's bare
`::T` parameter takes `Mu` (`R[Mu]`, `class C does R[Mu]`, `^add_role`), and a
core enum variant (`Same`, `Kept`, `SIGINT`) is a known term in an `EVAL`
inside a module routine, where it used to be undeclared or a plain string.
