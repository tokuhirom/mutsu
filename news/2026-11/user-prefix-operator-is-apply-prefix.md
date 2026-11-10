# A user-declared prefix operator is `ApplyPrefix` in the RakuAST tree

`sub prefix:<⥷>` applied as `⥷ <name>` showed up as `Call::Name` in `.AST`; rakudo has
`ApplyPrefix(prefix => Prefix, operand)`. The view now renders it so, the lowering turns an
`ApplyPrefix` over a symbol the core does not define back into the routine call, and a slang's
`PREFIX-EXPR` override now sees such an application. That is what Qwiratry's Topic slang rewrites
(`⪪ <owner>` becomes `qwiratry-bare-child($_, <owner>)`), so `05-navigation-topic` and
`05-navigation-tree` match raku.
