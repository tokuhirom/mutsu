# A bare `.= meth` is the topic call it is in RakuAST

`.=uc` written with no target is the topic's `.=`. rakudo 2026.09 builds
`Term::TopicCall` over a `Call::Method` whose `dispatch` is `.=`. `$_ .= uc`,
written with its target, is `ApplyDottyInfix`. mutsu rendered both as the
dotty infix, and in expression position (`$x andthen .=new: 43`, `say(.=uc)`)
as a plain `$_ = $_.new(43)` assignment.

The parser now keeps the bare form as `ParsedExpr::CompoundAssign` with
`CompoundMeta::TopicDotty`. `compound_lower` lowers it the way it lowers
`$_ .= meth`:
- as a statement, through `__mutsu_topic_dotassign`, so `given @a { .=uc }`
  still writes through to `@a`;
- in expression position, through the `.=` marker.

`rakuast::lower` reads `Term::TopicCall` with a `.=` dispatch into the same
node (#14948).

The expression form now carries the `.=` marker. So a bare `.=uc` is allowed
unparenthesized in a ternary branch (`1 ?? .=uc !! 2`), as on rakudo. It used
to die with "Precedence of = is too loose".

`t/rakuast/rakuast-topic-dotty-call-node.t` pins the shapes and the results.
