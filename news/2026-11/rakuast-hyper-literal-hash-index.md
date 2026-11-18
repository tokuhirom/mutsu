# RakuAST: `%h>>.<k>` is a hyper literal hash index

`%h>>.<k>` and `%h>>.<a b>` used to appear in `.AST` as `MetaPostfix::Hyper` over an
explicit `Call::Method` named `AT-KEY`, which also changed `.DEPARSE`. The parser now
keeps the raw key text on the key, and the view produces
`MetaPostfix::Hyper(Postcircumfix::LiteralHashIndex)` with the keys as one word quote,
matching rakudo 2026.11. The hand-built tree lowers back to the same hyper call, so
`EVAL($ast)` behaves as before.
