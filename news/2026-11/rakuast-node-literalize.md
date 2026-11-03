# `RakuAST::Node.literalize`

A node that is a constant now answers its value: literals, quoted strings
without interpolation, the constant terms (`pi`, `e`, `tau`, `Inf`, `NaN`,
`True`, `Bool::True`, type names), fat arrows and colonpairs, parenthesised and
comma lists (`[...]` answers a `List`, as rakudo does), and any core infix,
prefix or postfix operator over constants (`but` included). Everything else
answers the `RakuAST::Node::CannotLiteralize` type object, which rakudo
returns rather than throws. The decision is made from the node's shape alone
(`rakuast/literalize.rs`), so no stateful operator (`=`, `++`, `ff`, `does`)
and no call is ever run; the value then comes from the ordinary `EVAL` path.
Known difference: a `{a => 1}` hash composer is not literalized (rakudo
answers a `Map`). `locate-node` (needs `.origin` coverage, #15780) and
`rakudoc` (#14880) stay open under #14916.
